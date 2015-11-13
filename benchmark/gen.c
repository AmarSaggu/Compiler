#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int get_num()
{
	return (1 + rand() % 10);
}

char get_op()
{
	switch (rand() % 2) {
		case 0: return '+';
		case 1: return '-';
	}
}

int main(void)
{
	srand(time(NULL));

	printf("x = %d", get_num());
	for (int i = 0; i < 10000 - 1; i++) {
		char op = get_op();
		
		int a;

			a = get_num();

		printf(" %c %d", op, a);
	}

	puts("");
	
	return 0;
}
