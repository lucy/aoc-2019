package main

import (
	"fmt"
	"io"
	"log"
	"sync"
)

func run(m []int64, r <-chan int64, w chan<- int64) {
	pc := int64(0)
	next := func() int64 { x := pc; pc++; return x }
	in := func(f int64) int64 {
		switch f {
		case 1:
			return m[next()]
		case 0:
			return m[m[next()]]
		default:
			panic(fmt.Sprintf("invalid flag: %d", f))
		}
	}
	out := func(x int64) { m[m[next()]] = x }
	for {
		ins := m[next()]
		op := ins % 100
		f1 := ins / 100 % 10
		f2 := ins / 1000 % 10
		//m3 := ins / 10000 % 10
		switch op {
		case 1:
			in1 := in(f1)
			in2 := in(f2)
			out(in1 + in2)
		case 2:
			in1 := in(f1)
			in2 := in(f2)
			out(in1 * in2)
		case 3:
			x := <-r
			out(x)
		case 4:
			in1 := in(f1)
			w <- in1
		case 5:
			in1 := in(f1)
			in2 := in(f2)
			if in1 != 0 {
				pc = in2
			}
		case 6:
			in1 := in(f1)
			in2 := in(f2)
			if in1 == 0 {
				pc = in2
			}
		case 7:
			in1 := in(f1)
			in2 := in(f2)
			if in1 < in2 {
				out(1)
			} else {
				out(0)
			}
		case 8:
			in1 := in(f1)
			in2 := in(f2)
			if in1 == in2 {
				out(1)
			} else {
				out(0)
			}
		case 99:
			return
		}
	}
}

func runWith(a []int64, input int64) int64 {
	r, w := make(chan int64), make(chan int64)
	go func() { run(a, r, w); close(w) }()
	out := int64(0)
	for {
		select {
		case r <- input:
		case x, ok := <-w:
			if !ok {
				return out
			}
			out = x
		}
	}
}

func run2(a []int64, b []int64) {
	r1, w1 := make(chan int64), make(chan int64)
	r2, w2 := make(chan int64), make(chan int64)
	end := make(chan struct{})
	wg := sync.WaitGroup{}
	wg.Add(2)
	go func() { run(a, r1, w1); wg.Done() }()
	go func() { run(b, r2, w2); wg.Done() }()
	go func() {
		for {
			select {
			case x := <-w1:
				fmt.Println("send 1->2", x)
				r2 <- x
			case x := <-w2:
				fmt.Println("send 2->1", x)
				r1 <- x
			case <-end:
				return
			}
		}
	}()
	wg.Wait()
	close(end)
}

func main() {
	a, x := []int64{}, int64(0)
	for {
		n, err := fmt.Scanf("%d", &x)
		if err != nil && err != io.EOF {
			log.Fatal(err)
		}
		if n != 1 {
			break
		}
		a = append(a, x)
	}
	b := make([]int64, len(a))
	copy(b, a)
	fmt.Println(runWith(b, 1))
	copy(b, a)
	fmt.Println(runWith(b, 5))
}
