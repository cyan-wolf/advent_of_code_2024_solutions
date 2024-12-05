#include <stdio.h>
#include <assert.h>
#include <string.h>

#define LINE_AMT 200
#define LINE_LEN 200

int main() {
	FILE *file = fopen("input.txt", "r");
	char *lines[LINE_AMT];
	char currentLine[LINE_LEN];

	assert(file != NULL);

	int linePos = 0;

	while (fgets(currentLine, sizeof(currentLine), file) != NULL) {
		lines[linePos] = currentLine;
		linePos++;
	}

	int rows = linePos;
	int cols = strlen(lines[0]);

	printf("rows: %d, cols: %d", rows, cols);

	// TODO
    // ...

	return 0;
}
