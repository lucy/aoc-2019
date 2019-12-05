#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define prog_len 1024

long run(long a[prog_len], long n, long input) {
	int pc = 0;
	long output = 0;
	for (;;) {
		if (pc >= n) {
			fprintf(stderr, "pc overflow: %d\n", pc);
		}
		long ins = a[pc++];
		long op = ins/    1%100;
		long m1 = ins/  100%10;
		long m2 = ins/ 1000%10;
		//long m3 = ins/10000%10;
#define input(m) (m ? a[pc++] : a[a[pc++]]);
		if (op == 1) {
			long in1 = input(m1);
			long in2 = input(m2);
			long out = a[pc++];
			a[out] = in1 + in2;
			continue;
		}
		if (op == 2) {
			long in1 = input(m1);
			long in2 = input(m2);
			long out = a[pc++];
			a[out] = in1 * in2;
			continue;
		}
		if (op == 3) {
			long out = a[pc++];
			a[out] = input;
			continue;
		}
		if (op == 4) {
			long in = input(m1);
			//printf("output: %ld\n", in);
			output = in;
			continue;
		}
		if (op == 5) {
			long in1 = input(m1);
			long in2 = input(m2);
			if (in1 != 0) {
				pc = in2;
			}
			continue;
		}
		if (op == 6) {
			long in1 = input(m1);
			long in2 = input(m2);
			if (in1 == 0) {
				pc = in2;
			}
			continue;
		}
		if (op == 7) {
			long in1 = input(m1);
			long in2 = input(m2);
			long out = a[pc++];
			if (in1 < in2) {
				a[out] = 1;
			} else {
				a[out] = 0;
			}
			continue;
		}
		if (op == 8) {
			long in1 = input(m1);
			long in2 = input(m2);
			long out = a[pc++];
			if (in1 == in2) {
				a[out] = 1;
			} else {
				a[out] = 0;
			}
			continue;
		}
		if (op == 99) {
			goto end;
		}
		fprintf(stderr, "invalid opcode: %ld\n", op);
		goto end;
	}
end:
	return output;
}

int main(void) {
	long a[prog_len], b[prog_len], n = 0, x;
	while (scanf("%ld,", &x) == 1)
		b[n++] = x;
	memcpy(a, b, sizeof(a));
	long p1 = run(a, n, 1);
	memcpy(a, b, sizeof(a));
	long p2 = run(a, n, 5);
	printf("%ld\n%ld\n", p1, p2);
}
