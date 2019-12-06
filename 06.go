package main

import (
	"fmt"
	"strings"
)

func search(g map[string][]string, start, end string) int {
	d := -1
	s := map[string]struct{}{}
	q := []string{start}
	for len(q) != 0 {
		nq := []string{}
		for _, p := range q {
			for _, c := range g[p] {
				if c == end {
					return d
				}
				if _, ok := s[c]; !ok {
					nq = append(nq, c)
				}
				s[c] = struct{}{}
			}
		}
		q = nq
		d += 1
	}
	return d
}

func main() {
	s := ""
	g := map[string][]string{}
	u := map[string][]string{}
	for {
		n, _ := fmt.Scanf("%s", &s)
		if n != 1 {
			break
		}
		v := strings.Split(s, ")")
		a, b := v[0], v[1]
		g[b] = append(g[b], a)
		u[a] = append(u[a], b)
		u[b] = append(u[b], a)
	}
	t := 0
	for k := range u {
		t += search(g, k, "")
	}
	fmt.Println(t)
	fmt.Println(search(u, "YOU", "SAN"))
}
