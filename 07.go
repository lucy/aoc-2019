package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"strings"
	"sync"
)

func run(m []int64, r <-chan int64, w chan<- int64) {
	pc := int64(0)
	next := func() int64 { pc++; return pc - 1 }
	in := func(f int64) int64 {
		switch f {
		case 1:
			return m[next()]
		case 0:
			return m[m[next()]]
		default:
			panic(fmt.Sprintf("invalid mode: %d", f))
		}
	}
	out := func(x int64) { m[m[next()]] = x }
	for {
		ins := m[next()]
		f1, f2 := ins/100%10, ins/1000%10
		//f3 := ins / 10000 % 10
		switch ins % 100 {
		case 1:
			in1, in2 := in(f1), in(f2)
			out(in1 + in2)
		case 2:
			in1, in2 := in(f1), in(f2)
			out(in1 * in2)
		case 3:
			out(<-r)
		case 4:
			w <- in(f1)
		case 5:
			in1, in2 := in(f1), in(f2)
			if in1 != 0 {
				pc = in2
			}
		case 6:
			in1, in2 := in(f1), in(f2)
			if in1 == 0 {
				pc = in2
			}
		case 7:
			in1, in2 := in(f1), in(f2)
			if in1 < in2 {
				out(1)
			} else {
				out(0)
			}
		case 8:
			in1, in2 := in(f1), in(f2)
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

func perm(a []int64, f func([]int64), i int) {
	if i > len(a) {
		f(a)
		return
	}
	perm(a, f, i+1)
	for j := i + 1; j < len(a); j++ {
		a[i], a[j] = a[j], a[i]
		perm(a, f, i+1)
		a[i], a[j] = a[j], a[i]
	}
}

func p1(m []int64, p []int64) int64 {
	w := make(chan int64)
	a := w
	for _, x := range p {
		b := make([]int64, len(m))
		copy(b, m)
		r := w
		w = make(chan int64)
		go run(b, r, w)
		r <- x
	}
	a <- 0
	return <-w
}

func p2(m []int64, p []int64) int64 {
	w := make(chan int64)
	a := w
	wg := sync.WaitGroup{}
	wg.Add(len(p))
	for _, x := range p {
		b := make([]int64, len(m))
		copy(b, m)
		r := w
		w = make(chan int64)
		go func() { run(b, r, w); wg.Done() }()
		r <- x
	}
	defer close(w) // kill pipe after return
	go func() {    // pipe last to first
		for {
			v, ok := <-w
			if !ok {
				return
			}
			a <- v
		}
	}()
	a <- 0
	wg.Wait()
	return <-a
}

func w(m []int64, p []int64, f func([]int64, []int64) int64) {
	max := int64(0)
	perm(p, func(p []int64) {
		x := f(m, p)
		if x > max {
			max = x
		}
	}, 0)
	fmt.Println(max)
}

func main() {
	input, _ := ioutil.ReadAll(os.Stdin)
	s := string(input)
	a := []int64{}
	for _, s := range strings.Split(s, ",") {
		x := int64(0)
		n, err := fmt.Sscanf(s, "%d", &x)
		if err != nil && err != io.EOF {
			log.Fatal(err)
		}
		if n != 1 {
			break
		}
		a = append(a, x)
	}
	w(a, []int64{0, 1, 2, 3, 4}, p1)
	w(a, []int64{0, 1, 2, 3, 4}, p2) // works too
	w(a, []int64{5, 6, 7, 8, 9}, p2)
}
