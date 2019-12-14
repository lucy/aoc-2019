package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

type comp struct {
	n string
	c int
}

type reaction struct {
	r comp
	i []comp
}

func pc(s string) (c comp) { fmt.Sscanf(s, "%d %s", &c.c, &c.n); return }
func divUp(x, d int) int   { return (x + d - 1) / d }

func run(m map[string]reaction, n int) int {
	ore := 0
	q := []comp{comp{"FUEL", n}}
	lefts := map[string]int{}
	for len(q) != 0 {
		var x comp
		x, q = q[len(q)-1], q[:len(q)-1]
		if x.n == "ORE" {
			ore += x.c
			continue
		}
		r := m[x.n]
		need := x.c
		makes := r.r.c
		left, _ := lefts[x.n]
		if need <= left {
			left -= need
			lefts[x.n] = left
		} else {
			need -= left
			toMake := (need + makes - 1) / makes
			left = (need+makes-1)/makes*makes - need
			lefts[x.n] = left
			for _, in := range r.i {
				q = append(q, comp{in.n, in.c * toMake})
			}
		}
	}
	return ore
}

func main() {
	b, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}
	m := map[string]reaction{}
	ff := func(r rune) bool { return r == '\n' }
	for _, line := range strings.FieldsFunc(string(b), ff) {
		var r reaction
		s := strings.Split(line, " => ")
		for _, i := range strings.Split(s[0], ", ") {
			r.i = append(r.i, pc(i))
		}
		r.r = pc(s[1])
		m[r.r.n] = r
	}
	fmt.Println(run(m, 1))
	/* manual binary search */
	fmt.Println(run(m, 3445249) - 1000000000000)
}
