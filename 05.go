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

func runWith(a []int64, input int64) {
	ch := make(chan int64)
	go func() { run(a, ch, ch); close(ch) }()
	ch <- input
	for {
		if x, ok := <-ch; ok {
			fmt.Println(input, x)
		} else {
			break
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
	input, _ := ioutil.ReadAll(os.Stdin)
	s := string(input)
	//u := [][]int64{}
	//for _, s :=  range strings.Split(s, "\n") {
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
	// u =append(u, a)
	//}
	b := make([]int64, len(a))
	copy(b, a)
	runWith(b, 1)
	copy(b, a)
	runWith(b, 5)
}
