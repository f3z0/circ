package circ

// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

import (
	"errors"
	"unicode/utf8"
	"io"
	"fmt"
	"bytes"
)

var EOVERLAP = errors.New("runtime error: tried to write over unread portion of circular buffer, write offset overlapped read offset.")

// A Buffer is a fixed-sized circular buffer of bytes with Read and Write methods.
// The zero value for Buffer is an empty buffer ready to use.
type Buffer struct {
	buf      []byte // contents are the bytes buf[off : len(buf)]
	rOff     int    // read at &buf[mod(rOff)],
	wOff     int    // write at &buf[mod(wOff)]
	lastRead readOp // last read operation, so that Unread* can work correctly.

}

func max(a int, b int) int {
	if a >= b {
		return a
	} else {
		return b
	}
}

func min(a int, b int) int {
	if a <= b {
		return a
	} else {
		return b
	}
}

func mod(a int, b int) (int) {
	return a-(a/b)*b
}

// Len returns the number of bytes of the unread portion of the buffer;
// b.Len() == len(b.Bytes()).
func (b *Buffer) Len() int {
	return min(b.wOff-b.rOff, len(b.buf))
}



// maximum write possible before overlapping unread bytes
func (b *Buffer) writeCapacity() (int){
	return len(b.buf) - mod(b.wOff, len(b.buf)) + mod(b.rOff, len(b.buf))
}

// --

// The readOp constants describe the last action performed on
// the buffer, so that UnreadRune and UnreadByte can check for
// invalid usage. opReadRuneX constants are chosen such that
// converted to int they correspond to the rune size that was read.
type readOp int8

const (
	opRead      readOp = -1 // Any other read operation.
	opInvalid          = 0  // Non-read operation.
	opReadRune1        = 1  // Read rune of size 1.
	opReadRune2        = 2  // Read rune of size 2.
	opReadRune3        = 3  // Read rune of size 3.
	opReadRune4        = 4  // Read rune of size 4.
)

// ErrTooLarge is passed to panic if memory cannot be allocated to store data in a buffer.
var ErrTooLarge = errors.New("circ.bytes..Buffer: too large")

// Bytes returns a slice of length b.Len() holding the unread portion of the buffer.
// The slice is valid for use only until the next buffer modification (that is,
// only until the next call to a method like Read, Write, Reset, or Truncate).
// The slice aliases the buffer content at least until the next buffer modification,
// so immediate changes to the slice will affect the result of future reads.
func (b *Buffer) Bytes() (p []byte) {
	p = make([]byte, b.Len())
	if b.rOff <= b.wOff {
		copy(p, b.buf[mod(b.rOff, len(b.buf)):mod(b.wOff, len(b.buf))])	// [---|r|=====|w|----|]
	} else {
		n := copy(p[0:], b.buf[mod(b.rOff, len(b.buf)):len(b.buf)]) 		 // [---|w|-----|r|====|]
		copy(p[n:], b.buf[0:b.wOff]) // [===|w|-----|r|----|]
	}
	return
}

// String returns the contents of the unread portion of the buffer
// as a string. If the Buffer is a nil pointer, it returns "<nil>".
func (b *Buffer) String() string {
	if b == nil {
		// Special case, useful in debugging.
		return "<nil>"
	}
	return string(b.Bytes())
}


// Cap returns the capacity of the buffer's underlying byte slice, that is, the
// total space allocated for the buffer's data.
func (b *Buffer) Cap() int { return cap(b.buf) }


// Reset resets the buffer to be empty,
// but it retains the underlying storage for use by future writes.
// Reset is the same as Truncate(0).
func (b *Buffer) Reset() {
	b.buf = b.buf[:0]
	b.rOff = 0
	b.wOff = 0
	b.lastRead = opInvalid
}

// Write appends the contents of p to the buffer, circling the buffer as
// needed. If write is successful return value n is length of p and err
// is nil. If the length of p would cause buffer to overlap unread portion
// of circular buffer than n will be 0 and err will be returned.
func (b *Buffer) Write(p []byte) (n int, err error) {
	b.lastRead = opInvalid
	if len(p) >= len(b.buf)-b.Len() { return 0, EOVERLAP }
	if mod(b.wOff, len(b.buf))+len(p) > len(b.buf) {
		n = copy(b.buf[mod(b.wOff, len(b.buf)):], p[:len(b.buf)-mod(b.wOff, len(b.buf))])
		n += copy(b.buf[:len(p)-n], p[n:])
	} else {
		n = copy(b.buf[mod(b.wOff, len(b.buf)):], p)
	}
	b.wOff += n
	return n, nil
}

// WriteString appends the contents of s to the buffer, circling the buffer as
// needed. If write is successful return value n is length of p and err
// is nil. If the length of p would cause buffer to overlap unread portion
// of circular buffer than n will be 0 and err will be returned.
func (b *Buffer) WriteString(s string) (n int, err error) {
	return b.Write([]byte(s))
}

// MinRead is the minimum slice size passed to a Read call by
// Buffer.ReadFrom. As long as the Buffer has at least MinRead bytes beyond
// what is required to hold the contents of r, ReadFrom will not grow the
// underlying buffer.
const MinRead = 512

// ReadFrom reads data from r until EOF and appends it to the buffer, growing
// the buffer as needed. The return value n is the number of bytes read. Any
// error except io.EOF encountered during the read is also returned. If the
// buffer becomes too large, ReadFrom will panic with ErrTooLarge.
func (b *Buffer) ReadFrom(r io.Reader) (n int64, err error) {
	b.lastRead = opInvalid
	// If buffer is empty, reset to recover space.
	if len(b.buf) == 0 {
		b.Reset()
	}
	for {
		var e error
		var m int
		var µ int
		if len(b.buf)-b.wOff < MinRead {
			// not enough space at end
			newBuf := make([]byte, MinRead)
			µ, e = r.Read(newBuf)
			if e == nil {
				m, e = b.Write(newBuf[:µ])
			}
		} else if MinRead < b.Len() {
			m, e = r.Read(b.buf[b.wOff:b.wOff+MinRead])
		}

		n += int64(m)
		if e == io.EOF {
			break
		}
		if e != nil {
			return n, e
		}
	}
	return n, nil // err is EOF, so return nil explicitly
}


// WriteTo writes data to w until the buffer is drained or an error occurs.
// The return value n is the number of bytes written; it always fits into an
// int, but it is int64 to match the io.WriterTo interface. Any error
// encountered during the write is also returned.
func (b *Buffer) WriteTo(w io.Writer) (n int64, err error) {
	b.lastRead = opInvalid
	if b.Len() > 0 {
		var e error
		m := 0
		nBytes := b.Len()
		if mod(b.rOff, len(b.buf)) <=  mod(b.wOff, len(b.buf)) {
			m, e = w.Write(b.buf[mod(b.rOff, len(b.buf)):mod(b.wOff, len(b.buf))])		// [---|r|=====|w|----|]
		} else {
			m, e = w.Write(b.buf[mod(b.rOff, len(b.buf)):len(b.buf)])	// [---|w|-----|r|====|]
			if e == nil {
				µ := 0
				µ, e = w.Write(b.buf[0:mod(b.wOff, len(b.buf))])			// [===|w|-----|r|----|]
				m += µ
			}
		}
		if m > nBytes {
			panic("circ.Buffer.WriteTo: invalid Write count")
		}
		b.rOff += m
		n = int64(m)
		if e != nil {
			return n, e
		}
		// all bytes should have been written, by definition of
		// Write method in io.Writer
		if m != nBytes {
			return n, io.ErrShortWrite
		}
	}
	return
}

// WriteByte appends the byte c to the buffer, growing the buffer as needed.
// The returned error is always nil, but is included to match bufio.Writer's
// WriteByte. If the buffer becomes too large, WriteByte will panic with
// ErrTooLarge.
func (b *Buffer) WriteByte(c byte) error {
	b.lastRead = opInvalid
	if len(b.buf) - b.Len() == 0 { return EOVERLAP }
	b.buf[b.wOff] = c
	b.wOff++
	return nil
}

// WriteRune appends the UTF-8 encoding of Unicode code point r to the
// buffer, returning its length and an error, which is always nil but is
// included to match bufio.Writer's WriteRune. The buffer is grown as needed;
// if it becomes too large, WriteRune will panic with ErrTooLarge.
func (b *Buffer) WriteRune(r rune) (n int, err error) {
	b.WriteByte(byte(r))

	if r < utf8.RuneSelf {
		b.WriteByte(byte(r))
		return 1, nil
	}

	if len(b.buf) - b.Len() < utf8.UTFMax { return 0, EOVERLAP }
	b.lastRead = opInvalid

	rs := make([]byte, utf8.UTFMax)
	n = utf8.EncodeRune(rs, r)
	return b.Write(rs)
}

// Read reads the next len(p) bytes from the buffer or until the buffer
// is drained. The return value n is the number of bytes read. If the
// buffer has no data to return, err is io.EOF (unless len(p) is zero);
// otherwise it is nil.
func (b *Buffer) Read(p []byte) (n int, err error) {
	unr := b.Len()
	if unr == 0 {
		return 0, io.EOF
	}

	s := min(unr, cap(p))
	if mod(b.rOff, len(b.buf))+s > len(b.buf) {
		m := len(b.buf)-mod(b.rOff, len(b.buf))


		n = copy(p[0:m], b.buf[mod(b.rOff, len(b.buf)):len(b.buf)])
		if m != n {
			panic(errors.New(fmt.Sprintf("Invalid byte count while reading from circular buffer, %d != %d.", m, n)))
		}

	}

	st := mod(b.rOff+n, len(b.buf))
	en  := st + s - n
	n += copy(p[n:],b.buf[st:en])
	b.rOff += n
	return
}


// Next returns a slice containing the next n bytes from the buffer,
// advancing the buffer as if the bytes had been returned by Read.
// If there are fewer than n bytes in the buffer, Next returns the entire buffer.
// The slice is only valid until the next call to a read or write method.
func (b *Buffer) Next(n int) (p []byte) {
	b.lastRead = opInvalid
	m := b.Len()
	if n > m { n = m }
	p = make([]byte, n)
	m, e := b.Read(p)
	if m != n || e != nil { panic(e) }
	b.rOff += n
	if n > 0 { b.lastRead = opRead }
	return p
}

// ReadByte reads and returns the next byte from the buffer.
// If no byte is available, it returns error io.EOF.
func (b *Buffer) ReadByte() (byte, error) {
	b.lastRead = opInvalid
	if b.Len() == 0 {
		return 0, io.EOF
	}
	c := b.buf[mod(b.rOff, len(b.buf))]
	b.rOff++
	b.lastRead = opRead
	return c, nil
}

// ReadRune reads and returns the next UTF-8-encoded
// Unicode code point from the buffer.
// If no bytes are available, the error returned is io.EOF.
// If the bytes are an erroneous UTF-8 encoding, it
// consumes one byte and returns U+FFFD, 1.
func (b *Buffer) ReadRune() (r rune, size int, err error) {
	b.lastRead = opInvalid
	if b.Len() == 0 {
		return 0, 0, io.EOF
	}
	c := b.buf[mod(b.rOff, len(b.buf))]
	if c < utf8.RuneSelf {
		b.rOff++
		b.lastRead = opReadRune1
		return rune(c), 1, nil
	}

	var n int
	if mod(b.rOff, len(b.buf)) > mod(b.wOff, len(b.buf)) {
		r, n = utf8.DecodeRune(append(b.buf[mod(b.rOff, len(b.buf)):], b.buf[:mod(b.wOff, len(b.buf))]...))
	} else {
		r, n = utf8.DecodeRune(b.buf[mod(b.rOff, len(b.buf)):mod(b.wOff, len(b.buf))])
	}

	b.rOff += n
	b.lastRead = readOp(n)
	return r, n, nil
}

// UnreadRune unreads the last rune returned by ReadRune.
// If the most recent read or write operation on the buffer was
// not a successful ReadRune, UnreadRune returns an error.  (In this regard
// it is stricter than UnreadByte, which will unread the last byte
// from any read operation.)
func (b *Buffer) UnreadRune() error {
	if b.lastRead <= opInvalid {
		return errors.New("bytes.Buffer: UnreadRune: previous operation was not a successful ReadRune")
	}
	if b.rOff >= int(b.lastRead) {
		b.rOff -= int(b.lastRead)
	}
	b.lastRead = opInvalid
	return nil
}

// UnreadByte unreads the last byte returned by the most recent successful
// read operation that read at least one byte. If a write has happened since
// the last read, if the last read returned an error, or if the read read zero
// bytes, UnreadByte returns an error.
func (b *Buffer) UnreadByte() error {
	if b.lastRead == opInvalid {
		return errors.New("circ.Buffer: UnreadByte: previous operation was not a successful read")
	}
	b.lastRead = opInvalid
	if b.rOff > 0 {
		b.rOff--
	}
	return nil
}


// ReadBytes reads until the first occurrence of delim in the input,
// returning a slice containing the data up to and including the delimiter.
// If ReadBytes encounters an error before finding a delimiter,
// it returns the data read before the error and the error itself (often io.EOF).
// ReadBytes returns err != nil if and only if the returned data does not end in
// delim.
func (b *Buffer) ReadBytes(delim byte) (line []byte, err error) {
	slice, err := b.readSlice(delim)
	// return a copy of slice. The buffer's backing array may
	// be overwritten by later calls.
	line = append(line, slice...)
	return
}


// readSlice is like ReadBytes but returns a reference to internal buffer data.
func (b *Buffer) readSlice(delim byte) (line []byte, err error) {
	var i int
	if mod(b.rOff, len(b.buf)) > mod(b.wOff, len(b.buf)) {
		i = bytes.IndexByte(b.buf[mod(b.rOff, len(b.buf)):], delim)
		if i < 0 {
			i = bytes.IndexByte(b.buf[:mod(b.wOff, len(b.buf))], delim)

			if i >= 0 { i += len(b.buf)-mod(b.rOff, len(b.buf))}
		}
	} else {
		i = bytes.IndexByte(b.buf[mod(b.rOff, len(b.buf)):mod(b.wOff, len(b.buf))], delim)
	}
	end := b.rOff + i + 1
	if i < 0 {
		end = b.wOff
		err = io.EOF
	}

	if mod(end, len(b.buf)) < mod(b.rOff, len(b.buf)) {
		line = b.buf[mod(b.rOff, len(b.buf)):len(b.buf)]
		line = append(line, b.buf[:mod(end, len(b.buf))]...)
	} else {
		line = b.buf[mod(b.rOff, len(b.buf)):mod(end, len(b.buf))]
	}
	b.rOff = end
	b.lastRead = opRead
	return line, err
}


// NewBuffer creates and initializes a new Buffer using buf as its
// initial contents. The new Buffer takes ownership of buf, and the
// caller should not use buf after this call. NewBuffer is intended to
// prepare a Buffer to read existing data. It can also be used to size
// the internal buffer for writing. To do that, buf should have the
// desired capacity but a length of zero.
//
// In most cases, new(Buffer) (or just declaring a Buffer variable) is
// sufficient to initialize a Buffer.
func NewBuffer(buf []byte) *Buffer { return &Buffer{buf: buf} }

// NewBufferString creates and initializes a new Buffer using string s as its
// initial contents. It is intended to prepare a buffer to read an existing
// string.
//
// In most cases, new(Buffer) (or just declaring a Buffer variable) is
// sufficient to initialize a Buffer.
func NewBufferString(s string) *Buffer { return &Buffer{buf: []byte(s)} }
