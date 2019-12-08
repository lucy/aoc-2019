#include <stdio.h>
#include <string.h>

#define W 25
#define H 6
#define Z (W*H)

int main(void) {
	char *s, p2[Z] = {0};
	if (scanf("%ms", &s) != 1) return 1;
	int min0 = Z+1, p1 = 0;
	for (int l = strlen(s)/Z; l--;) {
		int n0 = 0, n1 = 0, n2 = 0;
		for (int i = 0; i < Z; i++) {
			char c = s[i+l*Z];
			if (c != '2') p2[i] = c;
			n0 += c == '0';
			n1 += c == '1';
			n2 += c == '2';
		}
		if (n0 < min0) min0 = n0, p1 = n1*n2;
	}
	printf("%d\n", p1);
	for (int j = 0; j < H; j++) {
		for (int i = 0; i < W; i++)
			printf("%c", p2[j*W+i]-'0' ? '*' : ' ');
		printf("\n");
	}
}
