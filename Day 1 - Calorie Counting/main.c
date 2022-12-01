#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern uint64_t calorie_counting(const char* data);

int main(int argc, char* argv[]) {
	if (argc < 2) {
        fprintf(stderr, "Usage: ./%s <data_file>", argv[0]);
		return 1;
	}
	char * buffer = 0;
	long length;
	FILE * f = fopen(argv[1], "rb");
    if (!f) {
        perror("Could not open the file");
        return 1;
    }
	fseek (f, 0, SEEK_END);
    length = ftell (f);
    fseek (f, 0, SEEK_SET);

    buffer = malloc (length+1);
    if (!buffer) {
        fprintf(stderr, "Could not alloc mem\n");
        return 1;
    }

    fread (buffer, 1, length, f);
    fclose (f);
    buffer[length] = '\0';

    // guesstimate, you may need to adopt this for very large files
	uint64_t res = calorie_counting(buffer);
	printf("Calories: %u\n", (uint32_t) (res>>32));
	printf("Maximum 3: %u\n", (uint32_t) res);
	free(buffer);
	return 0;
}