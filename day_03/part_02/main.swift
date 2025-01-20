import Foundation

func readInput() -> String {
    // Get the current working directory.
    let currentDirectory = FileManager.default.currentDirectoryPath

    let filePath = currentDirectory + "/input.txt"

    do {
        // Read the contents of the file.
        let fileContents = try String(contentsOfFile: filePath, encoding: String.Encoding.ascii)
        return fileContents
    } catch {
        print("Error reading file: \(error.localizedDescription)")
        exit(1)
    }
}

// Used to represent the different possible instructions.
enum Token {
    case mul(a: Int, b: Int)
    case shouldDo(should: Bool)
}

// Used for scanning the text content from the file,
// parses tokens out of the given content.
struct Scanner {
    private var content: String
    private var tokens: [Token] = []
    private var pos = 0

    init(content: String) {
        self.content = content
    }

    mutating func scanContent() {
        while !self.out_of_bounds() {
            if self.peek() == "m" {
                self.advance()
                self.matchMul()
            }
            else if self.peek() == "d" {
                self.advance()
                self.matchDoDont()
            }
            else {
                self.advance()
            }
        }
    }

    // Matches a `mul` token such as "mul(1, 345)".
    mutating func matchMul() {
        if self.peek() != "u" {
            return
        }
        self.advance()

        if self.peek() != "l" {
            return
        }
        self.advance()

        if self.peek() != "(" {
            return
        }
        self.advance()

        // Parse first number argument.
        let num1: Int;
        if let n = self.matchNumber() {
            num1 = n
        } else {
            return
        }

        // Parse middle comma.
        if self.peek() != "," {
            return
        }
        self.advance()

        // Parse second number argument.
        let num2: Int;
        if let n = self.matchNumber() {
            num2 = n
        } else {
            return
        }

        if self.peek() != ")" {
            return
        }
        self.advance()

        // Build `mul` token.
        self.tokens.append(Token.mul(a: num1, b: num2))
    }

    // Parses a number.
    mutating func matchNumber() -> Int? {
        var numAcc = ""

        while self.peek().isNumber && numAcc.lengthOfBytes(using: String.Encoding.ascii) < 3 {
            numAcc.append(self.peek())
            self.advance()
        }

        // Only parse the number if the next tokens are valid.
        if self.peek() == "," || self.peek() == ")" {
            return Int(numAcc)
        }

        // Failure.
        return nil
    }

    // Matches a "do" or a "don't" instruction.
    mutating func matchDoDont() {
        if self.peek() != "o" {
            return
        }
        self.advance()

        let c = self.peek() 

        // Possible "do()" instruction.
        if c == "(" {
            self.advance()

            if self.peek() != ")" {
                return
            }
            self.advance()

            self.tokens.append(Token.shouldDo(should: true))
        }
        // Possible "don't()" instruction.
        else if c == "n" {
            self.advance()

            if self.peek() != "'" {
                return
            }
            self.advance()

            if self.peek() != "t" {
                return
            }
            self.advance()

            if self.peek() != "(" {
                return
            }
            self.advance()

            if self.peek() != ")" {
                return
            }
            self.advance()

            self.tokens.append(Token.shouldDo(should: false))
        }
        // Invalid instruction.
        else {
            return
        }
    }

    // Advances the scanner.
    mutating func advance() {
        self.pos += 1
    }

    func out_of_bounds() -> Bool {
        return self.pos >= self.content.count
    }

    // Get the current character in the content.
    // Returns `@` as a placeholder in case of out of bounds.
    func peek() -> Character {
        if self.out_of_bounds() {
            return "@"; // out of bounds
        }

        let index = self.content.index(self.content.startIndex, offsetBy: self.pos)
        return self.content[index]
    }

    func getTokens() -> [Token] {
        return self.tokens
    }
}

// Used for evaluating a series of tokens.
struct Evaluator {
    var tokens: [Token]
    var shouldExecuteMul = true

    init(tokens: [Token]) {
        self.tokens = tokens
    }

    mutating func evalTokens() -> Int {
        var sum = 0;

        for token in tokens {
            switch token {
                case let .mul(a, b): do {
                    if self.shouldExecuteMul {
                        sum += a * b
                    }
                }
                case let .shouldDo(should): do {
                    self.shouldExecuteMul = should
                }
            }
        }
        return sum
    }
}

var scanner = Scanner(content: readInput())
scanner.scanContent()

var evaluator = Evaluator(tokens: scanner.getTokens())
print(evaluator.evalTokens())
