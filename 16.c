#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static void p1(char *s, size_t z) {
	int *b = malloc(z * sizeof(*b));
	static const char base[] = {0, 1, 0, -1};
	const size_t bl = 4;
	for (size_t p = 0; p < 100; p++) {
		for (size_t n = 0; n < z; n++) {
			b[n] = 0;
			for (size_t i = 0; i < z; i++) {
				b[n] += s[i] * base[(i+1)/(n+1)%bl];
			}
			b[n] = abs(b[n])%10;
		}
		for (size_t i = 0; i < z; i++)
			s[i] = b[i];
	}
	for (size_t i = 0; i < 8; i++)
		printf("%d", s[i]);
	printf("\n");
	free(b);
}

static void p2(char *s, size_t zs) {
	size_t off = 0;
	for (size_t i = 0; i < 7; i++) off = off * 10 + s[i];
	size_t z = zs * 10000 - off;
	int *b = malloc(z * sizeof(*b));
	for (size_t i = 0; i < z; i++) {
		b[i] = s[(i+off)%zs];
	}
	for (int p = 0; p < 100; p++) {
		for (ssize_t i = z-1; i--;) b[i] += b[i+1];
		for (ssize_t i = z-1; i--;) b[i] %= 10;
	}
	for (size_t i = 0; i < 8; i++)
		printf("%d",b[i]);
	printf("\n");
}

int main(void) {
	char *s1;
	scanf("%ms", &s1);
	char *s2 = strdup(s1);
	size_t z = strlen(s1);
	for (size_t i = 0; i < z; i++) s1[i] = s2[i] -= '0';
	p1(s1, z);
	p2(s2, z);
}
