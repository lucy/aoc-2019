#include <stdio.h>

int main(void) {
	long x, p1 = 0, p2 = 0;
	while (scanf("%ld", &x) == 1) {
		p1 += x/3-2;
		while ((x = x/3-2) > 0) p2 += x;
	}
	printf("%ld\n%ld\n", p1, p2);
}
