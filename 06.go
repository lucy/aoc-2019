package main

import (
	"fmt"
	"runtime"
	"strings"
	"sync"
	"sync/atomic"
)

// FIXME: this is very stupid
func trace(m map[string]string, x string) []string {
	l := []string{x}
	i := 0
	for i != len(l) {
		k := l[i]
		if y, ok := m[k]; ok {
			l = append(l, y)
		}
		i++
	}
	return l
}

func traced(m map[string]string, x string) int {
	i := 0
	for {
		i++
		if y, ok := m[x]; ok {
			x = y
		} else {
			return i
		}
	}
}

func main() {
	return
	m := map[string]string{}
	l := make([]string, 0, 2000)
	for {
		s := ""
		n, _ := fmt.Scanf("%s", &s)
		if n != 1 {
			break
		}
		v := strings.Split(s, ")")
		a, b := v[0], v[1]
		m[b] = a
		l = append(l, b)
	}
	wg := sync.WaitGroup{}
	n := runtime.NumCPU()
	//n = 1
	wg.Add(n - 1)
	pt := int32(0)
	for i := 0; i < n-1; i++ {
		go func(i int) {
			t := 0
			n := len(l) / n
			a := l[n*i : n*(i+1)]
			for _, k := range a {
				t += traced(m, k) - 1
			}
			atomic.AddInt32(&pt, int32(t))
			wg.Done()
		}(i)
	}
	t := 0
	a := l[len(l)/n*(n-1):]
	for _, k := range a {
		t += traced(m, k) - 1
	}
	wg.Wait()
	t += int(pt)
	fmt.Println(t)
	// FIXME: this is very stupid too
	you, san := trace(m, "YOU"), trace(m, "SAN")
	off := 0
	i, j := len(you)-1, len(san)-1
	for i != 0 && j != 0 {
		if you[i] == san[j] {
			off++
		}
		i--
		j--
	}
	x := you[len(you)-off-1]
	y := san[len(san)-off-1]
	for n := range you {
		if you[n] == x {
			i = n
			break
		}
	}
	for n := range san {
		if san[n] == y {
			j = n
			break
		}
	}
	fmt.Println(j + i)
}
