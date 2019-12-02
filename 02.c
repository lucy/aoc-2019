#include <stdio.h>
#include <string.h>

int main(void) {
	long a[1024], b[1024], n = 0, x;
	while (scanf("%ld,", &x) == 1)
		b[n++] = x;
	long p1 = 0, p2 = 0;
	for (int x = 0; x <= 99; x++) {
		for (int y = 0; y <= 99; y++) {
			memcpy(a, b, sizeof(a));
			a[1] = x, a[2] = y;
			for (int i = 0; a[i] != 99; i += 4) {
				long in1 = a[i+1], in2 = a[i+2], out = a[i+3];
				if (a[i] == 1)
					a[out] = a[in1] + a[in2];
				if (a[i] == 2)
					a[out] = a[in1] * a[in2];
			}
			if (x == 12 && y == 2)
				p1 = a[0];
			if (a[0] == 19690720)
				p2 = 100 * x + y;
		}
	}
	printf("%ld\n%ld\n", p1, p2);
}
