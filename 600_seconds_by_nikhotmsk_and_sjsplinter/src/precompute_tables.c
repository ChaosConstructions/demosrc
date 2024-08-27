#include <stdio.h>

#define FLOAT_TO_INT(x) ((x)>=0?(int)((x)+0.5):(int)((x)-0.5))

int main (void) {
	int i;
	float multiplier = 1.0;
	for (multiplier = 1.0; multiplier > -1.0; multiplier -= 0.1) {
		printf("\tBYTE ");
		for (i = 0; i < 32; i++) {
			float a = (i - 16) * multiplier;
			printf(", %i", ((int) a) + 16);
		}
		printf("\n");
	}
	
	
}
