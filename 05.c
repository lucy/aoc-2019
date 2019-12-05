#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct vm { long *m; long pc; long ins; };
struct call { long v; char t; };

#define VM_START  0
#define VM_END    1
#define VM_INPUT  2
#define VM_OUTPUT 3

void run(struct vm *vm, struct call *call) {
	static void *op_table[] = { [1] = &&ADD, [2] = &&MUL, [3] = &&I, [4] = &&O,
		[5] = &&JT, [6] = &&JF, [7] = &&LT, [8] = &&EQ, [99] = &&END };
	long pc = vm->pc, *m = vm->m, ins = vm->ins, in1, in2;
#define IN1 in1 = ins/100%10 ? m[pc++] : m[m[pc++]]
#define IN2 in2 = ins/1000%10 ? m[pc++] : m[m[pc++]]
#define OUT(v) m[m[pc++]] = v
#define NEXT ins = m[pc++]; goto *op_table[ins%100]
#define CALL vm->pc = pc; vm->ins = ins; return
	switch (call->t) {
	case VM_START: NEXT;
	case VM_END: goto END;
	case VM_INPUT: OUT(call->v); NEXT;
	case VM_OUTPUT: NEXT;
	}
ADD: IN1; IN2; OUT(in1 + in2); NEXT;
MUL: IN1; IN2; OUT(in1 * in2); NEXT;
JT: IN1; IN2; if (in1) pc = in2; NEXT;
JF: IN1; IN2; if (!in1) pc = in2; NEXT;
LT: IN1; IN2; OUT(in1 < in2); NEXT;
EQ: IN1; IN2; OUT(in1 == in2); NEXT;
I: call->t = VM_INPUT; CALL;
O: IN1; call->t = VM_OUTPUT; call->v = in1; CALL;
END: call->t = VM_END; CALL;
}

long run_with_input(long *m, long input) {
	long output = 0;
	struct vm vm = (struct vm) { .m = m, .pc = 0 };
	struct call call = {0};
	for (;;) {
		run(&vm, &call);
		switch (call.t) {
		case VM_END: return output;
		case VM_INPUT: call.v = input; continue;
		case VM_OUTPUT: output = call.v; continue;
		}
	}
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
	long p1 = run_with_input(b, 1);
	memcpy(b, a, sizeof(*a) * len);
	long p2 = run_with_input(b, 5);
	printf("%ld\n%ld\n", p1, p2);
}
