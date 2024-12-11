#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#define LINE_AMT 200
#define LINE_LEN 200

char getAt(char **grid, int rows, int cols, int r, int c) {
	if (r >= 0 && r < rows && c >= 0 && c < cols) {
		return grid[r][c];
	}
	// Represents out of bounds.
	return '$';
}

int matchNeighboring(char **grid, int rows, int cols, int posR, int posC) {
	int matches = 0;

	// Single direction buffers.
	char bufferR[5] = {'\0'};
	char bufferL[5] = {'\0'};
	char bufferU[5] = {'\0'};
	char bufferD[5] = {'\0'};

	// Diagonal buffers.
	char bufferUR[5] = {'\0'};
	char bufferUL[5] = {'\0'};
	char bufferDR[5] = {'\0'};
	char bufferDL[5] = {'\0'};

	// Store all the buffers in a single array.
	char *buffers[8] = {
		bufferR, bufferL, bufferU, bufferD,
		bufferUR, bufferUL, bufferDR, bufferDL,
	};

	// Fill in the buffers by reading in every direction.
	for (int offset = 0; offset < 4; offset++) {
		bufferR[offset] = getAt(grid, rows, cols, posR, posC + offset);
		bufferL[offset] = getAt(grid, rows, cols, posR, posC - offset);
		bufferU[offset] = getAt(grid, rows, cols, posR - offset, posC);
		bufferD[offset] = getAt(grid, rows, cols, posR + offset, posC);

		bufferUR[offset] = getAt(grid, rows, cols, posR - offset, posC + offset);
		bufferUL[offset] = getAt(grid, rows, cols, posR - offset, posC - offset);
		bufferDR[offset] = getAt(grid, rows, cols, posR + offset, posC + offset);
		bufferDL[offset] = getAt(grid, rows, cols, posR + offset, posC - offset);
	}

	// Every buffer that contains "XMAS" increased the match count.
	for (int i = 0; i < 8; i++) {
		if (strcmp(buffers[i], "XMAS") == 0) {
			matches++;
		}
	}

	return matches;
}

// Counts how many matches there are starting from an 'X'.
int countXmas(char **grid, int rows, int cols) {
	int count = 0;

	for (int r = 0; r < rows; r++) {
		for (int c = 0; c < cols; c++) {
			if (grid[r][c] == 'X') {
				int matches = matchNeighboring(grid, rows, cols, r, c);
				count += matches;
			}
		}
	}
	return count;
}

int main() {
	FILE *file = fopen("input.txt", "r");
	char *lines[LINE_AMT];
	char buffer[LINE_LEN];
	
	assert(file != NULL);

	int linePos = 0;

	while (fgets(buffer, sizeof(buffer), file) != NULL) {
		// Trim newline.
		buffer[strcspn(buffer, "\n")] = '\0';

		// Heap allocate a string to store the line.
		lines[linePos] = malloc(strlen(buffer) + 1);
		assert(lines[linePos] != NULL);

		// Copy the contents of the buffer.
		strcpy(lines[linePos], buffer);
		linePos++;
	}

	int rows = linePos;
	int cols = strlen(lines[0]);

	int count = countXmas(lines, rows, cols);

	printf("Count: %d\n", count);

	fclose(file);
	return 0;
}
