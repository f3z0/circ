package circ

// Copyright 2017 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
import (
	"time"
	"errors"
)

type Buffer struct {
	b []byte
	rOff int
	wOff int
	wTO time.Duration
	rTO time.Duration
}

func NewBuffer(b []byte) *Buffer{
	return &Buffer{
		b: b[:cap(b)],
	}
}

var ReadTimeout error = errors.New("Read timeout exceeeded.")
var WriteTimeout error = errors.New("Write timeout exceeeded.")

func (cb *Buffer) WriteTimeout(d time.Duration) {
	cb.wTO = d
}

func (cb *Buffer) ReadTimeout(d time.Duration) {
	cb.rTO = d
}

func (cb *Buffer) GetWriteTimeout() (d time.Duration) {
	return cb.wTO
}

func (cb *Buffer) GetReadTimeout() (d time.Duration) {
	return cb.rTO
}

func (cb *Buffer) Len() (int) {
	if(cb.b == nil) { cb.b = make([]byte, 4096, 4096) }
	return cb.wOff - cb.rOff
}

func (cb *Buffer) Cap() (int) {
	if(cb.b == nil) { cb.b = make([]byte, 4096, 4096) }
	return cap(cb.b)
}

func (cb *Buffer) Read(p []byte) (n int, e error) {
	if len(p) == 0 {return 0, nil}
	if(cb.b == nil) { cb.b = make([]byte, 4096, 4096) }
	unread := cb.wOff - cb.rOff
	for i:=0; unread == 0;i++ {
		sleepDur :=time.Duration(25)*time.Nanosecond
		if cb.rTO > 0 {
			sleepDur = minDur(sleepDur, cb.rTO)
		}
		if cb.rTO > 0 && time.Duration(i)*sleepDur >= cb.rTO {
			return 0, ReadTimeout
		}
		time.Sleep(sleepDur)
		unread = cb.wOff - cb.rOff
	}
	readSize := min(unread, len(p))
	rOff := mod(cb.rOff, len(cb.b))

	if rOff + readSize > len(cb.b) {
		c := copy(p, cb.b[rOff:len(cb.b)])
		d := (rOff + readSize) - len(cb.b)
		n = copy(p[c:], cb.b[0:d])
		n += c
	} else {
		n = copy(p, cb.b[rOff:rOff+readSize])
	}

	cb.rOff += n

	return
}

func (cb *Buffer) Write(p []byte) (n int, e error) {
	if len(p) == 0 {return 0, nil}
	if(cb.b == nil) { cb.b = make([]byte, 4096, 4096) }
	for i:=0; cb.Len()+len(p) > len(cb.b); i++ {
		sleepDur :=time.Duration(25)*time.Nanosecond
		if cb.wTO > 0 {
			sleepDur = minDur(sleepDur, cb.wTO)
		}
		if cb.wTO > 0 && time.Duration(i)*sleepDur >= cb.wTO {
			return 0, WriteTimeout
		}
		time.Sleep(sleepDur)
	}
	start := mod(cb.wOff, len(cb.b))
	end := mod(cb.wOff + len(p), len(cb.b))
	if end < start {
		n = copy(cb.b[start:len(cb.b)], p[0:len(cb.b)-start])
		n += copy(cb.b[0:end], p[n:len(p)])
	} else {
		n = copy(cb.b[start:end], p)
	}
	cb.wOff += n

	return
}

func mod(a int, b int) (int) {
	return a-(a/b)*b
}

func max(a int, b int) (int) {
	if a > b { return a} else {return b}
}

func min(a int, b int) (int) {
	if a < b { return a} else {return b}
}

func minDur(a time.Duration, b time.Duration) (time.Duration) {
	if a < b { return a} else {return b}
}


func comp(a []byte, b []byte) bool {
	if len(a) != len(b) {
		var first []byte
		var second []byte
		if len(a) > len(b) {
			first = a
			second = b
		} else {
			first = b
			second = a
		}
		for _, oct := range first[len(second):len(first)] {
			if oct != 0x0 {
				return false
			}
		}
		a = second
		b = first
	}
	for i, oct := range a {
		if oct != b[i] {
			return false
		}
	}
	return true
}
