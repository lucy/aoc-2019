#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define prog_len 1024

long run(long a[prog_len], long input) {
	long output = 0;
	static void *op_table[] = { [1] = &&ADD, [2] = &&MUL, [3] = &&OUTPUT,
		[4] = &&INPUT, [5] = &&JT, [6] = &&JF, [7] = &&LT, [8] = &&EQ,
		[99] = &&END };
	int pc = 0;
	long ins, in1, in2;
#define OP (ins%100)
#define IN1 in1 = ins/  100%10 ? a[pc++] : a[a[pc++]]
#define IN2 in2 = ins/ 1000%10 ? a[pc++] : a[a[pc++]]
#define IN3 in3 = ins/10000%10 ? a[pc++] : a[a[pc++]]
#define OUT(v) a[a[pc++]] = v
#define NEXT do {\
	ins = a[pc++]; \
	goto *op_table[OP]; \
} while (0)
	NEXT;
ADD:
	IN1; IN2;
	OUT(in1 + in2);
	NEXT;
MUL:
	IN1; IN2;
	OUT(in1 * in2);
	NEXT;
OUTPUT:
	OUT(input);
	NEXT;
INPUT:
	IN1;
	output = in1;
	NEXT;
JT:
	IN1; IN2;
	if (in1)
		pc = in2;
	NEXT;
JF:
	IN1; IN2;
	if (!in1)
		pc = in2;
	NEXT;
LT:
	IN1; IN2;
	OUT(in1 < in2);
	NEXT;
EQ:
	IN1; IN2;
	OUT(in1 == in2);
	NEXT;
END:
	return output;
}

int main(void) {
	size_t len = 0, cap = 1024;
	long *a = malloc(sizeof(*a) * cap), x;
	while (scanf("%ld,", &x) == 1) {
		a[len++] = x;
		if (len >= cap) a = realloc(a, sizeof(*a) * (cap *= 2));
	}
	long *b = malloc(sizeof(*a) * len);
	memcpy(b, a, sizeof(*a) * len);
	long p1 = run(b, 1);
	memcpy(b, a, sizeof(*a) * len);
	long p2 = run(b, 5);
	printf("%ld\n%ld\n", p1, p2);
}
