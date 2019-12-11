#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct vm {
	long *m;
	long pc, ins;
	long state, ch;
};

#define VM_START 0
#define VM_END   1
#define VM_RECV  2
#define VM_SEND  3

void run(struct vm *vm) {
	static void *op_table[] = { [1] = &&ADD, [2] = &&MUL, [3] = &&RECV,
		[4] = &&SEND, [5] = &&JT, [6] = &&JF, [7] = &&LT, [8] = &&EQ,
		[99] = &&END };
	long pc = vm->pc, *m = vm->m, ins = vm->ins, in1, in2;
#define IN1 in1 = ins/100%10 ? m[pc++] : m[m[pc++]]
#define IN2 in2 = ins/1000%10 ? m[pc++] : m[m[pc++]]
#define OUT(v) m[m[pc++]] = v
#define NEXT ins = m[pc++]; goto *op_table[ins%100]
#define RET(st) vm->pc = pc; vm->ins = ins; vm->state = st; return
	switch (vm->state) {
	case VM_START: NEXT;
	case VM_END: goto END;
	case VM_RECV: OUT(vm->ch); NEXT;
	case VM_SEND: NEXT;
	}
ADD: IN1; IN2; OUT(in1 + in2); NEXT;
MUL: IN1; IN2; OUT(in1 * in2); NEXT;
JT: IN1; IN2; if (in1) pc = in2; NEXT;
JF: IN1; IN2; if (!in1) pc = in2; NEXT;
LT: IN1; IN2; OUT(in1 < in2); NEXT;
EQ: IN1; IN2; OUT(in1 == in2); NEXT;
RECV: RET(VM_RECV);
SEND: IN1; vm->ch = in1; RET(VM_SEND);
END: RET(VM_END);
}

long run_with_input(long *m, long input) {
	long output = 0;
	struct vm vm = (struct vm) { .m = m, .pc = 0, .state = VM_START };
	for (;;) {
		run(&vm);
		switch (vm.state) {
		case VM_END: return output;
		case VM_RECV: vm.ch = input; continue;
		case VM_SEND: output = vm.ch; continue;
		}
	}
	return output;
}

long run2(long *m1, long *m2) {
	long ch = 0;
	struct vm vm1 = (struct vm) { .m = m1, .pc = 0, .state = VM_START };
	struct vm vm2 = (struct vm) { .m = m2, .pc = 0, .state = VM_START };
	struct vm *r = &vm1, *s = &vm2;
	for (;;) {
		run(r);
		switch (r->state) {
		case VM_END:
			if (s->state == VM_END)
				return ch;
			break;
		case VM_SEND:
			s->ch = r->ch;
			break;
		case VM_RECV:
			break;
		}
		struct vm *tmp = r;
		r = s, s = tmp;
	}
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
