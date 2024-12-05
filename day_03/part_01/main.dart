import 'dart:io';

class Scanner {
    String content;
    // Current position of the scanner.
    int curr = 0;
    // Accumulator for the sum of multiplication operations.
    int mulAcc = 0;

    Scanner(this.content);

    void scan() {
        while (this.curr < this.content.length) {
            var sym = this.peek();

            // If an "m" is encountered, start trying to parse a 'mul' operation.
            if (sym == "m") {
                this.curr++;
                this.scanMul();
                continue;
            }

            this.curr++;
        }
    }

    // Get the final result.
    int getResult() {
        return this.mulAcc;
    }

    // Scans a number; returns -1 if it fails.
    int scanNum() {
        var numAcc = "";

        while (numAcc.length <= 3) {
            if (!this.isDigit(this.peek())) {
                if (numAcc.length == 0) {
                    break;
                }
                // A number could be parsed.
                return int.parse(numAcc);
            }
            numAcc += this.peek();
            this.curr++;
        }
        // A number could not be parsed.
        return -1;
    }

    // Scan for a full 'mul' operation.
    void scanMul() {
        if (this.peek() != "u") {
            return;
        }
        this.curr++;

        if (this.peek() != "l") {
            return;
        }
        this.curr++;

        if (this.peek() != "(") {
            return;
        }
        this.curr++;

        int arg1 = this.scanNum();
        if (arg1 < 0) {
            return;
        }

        if (this.peek() != ',') {
            return;
        }
        this.curr++;

        int arg2 = this.scanNum();
        if (arg2 < 0) {
            return;
        }
        
        if (this.peek() != ")") {
            return;
        }
        this.curr++;

        int prod = arg1 * arg2;
        this.mulAcc += prod;
    }

    String peek() {
        if (this.curr < this.content.length) {
            //print(this.content[this.curr]);
            return this.content[this.curr];
        }
        return '\0';
    }

    bool isDigit(String s) => s.contains(RegExp(r'\d'));
}

void main() {
    File('input.txt').readAsString().then((String contents) {
        var sc = new Scanner(contents);
        sc.scan();

        print(sc.getResult());
    });
}
