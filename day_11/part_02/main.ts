import * as fs from 'node:fs';

function readInput(): number[] {
    const content = fs.readFileSync("input.txt", "utf-8")
        .split(" ")
        .map(parseFloat);

    return content;
}

function hasEvenNumberOfDigits(n: number) {
    if (n == 0) {
        return false;
    }
    // Calculates the number of digits of a non-zero number.
    const digitAmt = Math.ceil(Math.log10(Math.abs(n) + 1));
    return digitAmt % 2 == 0;
}

function splitNumWithEvenDigitAmt(n: number): number[] {
    const numStr = n.toString();

    const firstHalf = parseFloat(numStr.slice(0, numStr.length / 2));
    const secondHalf = parseFloat(numStr.slice(numStr.length / 2));

    return [firstHalf, secondHalf];
}

// Calculates the "pluto number" for a given number n and depth.
// Saves previously calculated pluto numbers in the `seen` map.
function calcPlutoNumber(n: number, depth: number, seen: Map<string, number>): number {
    let plutoNum = 0;
    
    if (depth == 0) {
        return 1;
    }
    // Memoization retrieval.
    else if (seen.has([n, depth].toString())) {
        return seen.get([n, depth].toString())!;
    }
    else if (n == 0) {
        const pluto = calcPlutoNumber(1, depth - 1, seen);
        seen.set([1, depth - 1].toString(), pluto);

        plutoNum += pluto;
    } 
    else if (hasEvenNumberOfDigits(n)) {
        const [fst, snd] = splitNumWithEvenDigitAmt(n);

        const plutoFst = calcPlutoNumber(fst, depth - 1, seen);
        seen.set([fst, depth - 1].toString(), plutoFst);

        const plutoSnd = calcPlutoNumber(snd, depth - 1, seen);
        seen.set([snd, depth - 1].toString(), plutoSnd);

        plutoNum += plutoFst + plutoSnd;
    }
    else {
        const pluto = calcPlutoNumber(n * 2024, depth - 1, seen);
        seen.set([n * 2024, depth - 1].toString(), pluto);

        plutoNum += pluto;
    }

    return plutoNum;
}

function main() {
    const input = readInput();
    const DEPTH = 75;
    // Use memoization to speed up solution.
    const seen = new Map<string, number>();

    let plutoNum = 0;

    for (const n of input) {
        plutoNum += calcPlutoNumber(n, DEPTH, seen);
    }
    console.log(plutoNum);
}

main()