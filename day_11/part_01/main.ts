import * as fs from 'node:fs';

type Linked = {
    value: number,
    next: Linked | null,
}

function readInput(): Linked | null {
    const content = fs.readFileSync("input.txt", "utf-8")
        .split(" ")
        .map(parseFloat);

    let curr: Linked | null = null;

    for (const num of content) {
        curr = {
            value: num,
            next: curr,
        };
    }

    // Input is never empty, so `curr` is never null here.
    return curr;
}

function hasEvenNumberOfDigits(n: number) {
    if (n == 0) {
        return false;
    }
    // Calculates the number of digits of a non-zero number.
    const digitAmt = Math.ceil(Math.log10(Math.abs(n) + 1));
    return digitAmt % 2 == 0;
}

function getLinkedLength(curr: Linked | null) {
    let len = 0;

    while (curr != null) {
        len++;
        curr = curr.next;
    }
    return len;
}

function main() {
    const ITERATIONS = 25;
    const head = readInput();
    
    for (let i = 0; i < ITERATIONS; i++) {
        let curr = head;

        // Traverse the entire list.
        while (curr != null) {
            // If the current value is 0, set it to 1.
            if (curr.value == 0) {
                curr.value = 1;
            }
            // If the current value has an even number of digits, split them 
            // down the middle and turn them into 2 new nodes.
            else if (hasEvenNumberOfDigits(curr.value)) {
                const numStr = curr.value.toString();

                const firstHalf = parseFloat(numStr.slice(0, numStr.length / 2));
                const secondHalf = parseFloat(numStr.slice(numStr.length / 2));

                curr.value = firstHalf;
                curr.next = {
                    value: secondHalf,
                    next: curr.next,
                };

                // Skip past the second half, since it's suppossed to 
                // be processed in the next iteration, not this one.
                curr = curr.next;
            }
            // Otherwise, multiply the current value by 2024.
            else {
                curr.value *= 2024;
            }

            curr = curr.next;
        }
    }

    console.log(getLinkedLength(head));
}

main()
