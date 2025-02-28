import std.stdio;
import std.string;
import std.file;
import std.algorithm;
import std.conv;
import std.math.exponential;
import std.format;

// Represents a computer from the problem statement.
class Computer
{
    int regA, regB, regC;
    int[] program;

    bool shouldIncrementInstPr = true;

    int instPtr = 0;
    string[] output = [];

    this(int regA, int regB, int regC, int[] program) 
    {
        this.regA = regA;
        this.regB = regB;
        this.regC = regC;
        this.program = program;
    }

    void runProgram()
    {
        while (instPtr < program.length)
        {
            runCurrInstruction();

            if (!shouldIncrementInstPr)
            {
                shouldIncrementInstPr = true;
                continue;
            }

            instPtr += 2;
        }
    }

    void runCurrInstruction() 
    {
        switch (program[instPtr])
        {
            case 0:
                adv();
                break;

            case 1:
                bxl();
                break;

            case 2:
                bst();
                break;

            case 3:
                jnz();
                break;

            case 4:
                bxc();
                break;

            case 5:
                out_();
                break;

            case 6:
                bdv();
                break;

            case 7:
                cdv();
                break;

            default:
                writeln("error: unknown instruction");
                break;
        }
    }

    int getLiteralOperand()
    {
        return program[instPtr + 1];
    }

    int getComboOperand()
    {
        auto operand = getLiteralOperand();

        switch (operand)
        {
            case 0, 1, 2, 3:
                return operand;

            case 4:
                return regA;

            case 5:
                return regB;

            case 6:
                return regC;

            default:
                writeln("error: invalid operand: ", operand);
                return -1;
        }
        
    }

    void adv() 
    {
        int denominator = pow(2, getComboOperand());
        regA /= denominator;
    }

    void bxl()
    {
        regB ^= getLiteralOperand();
    }
    
    void bst() 
    {
        regB = getComboOperand() % 8;
    }

    void jnz()
    {
        if (regA != 0)
        {
            instPtr = getLiteralOperand();
            shouldIncrementInstPr = false;
        }
    }

    void bxc()
    {
        regB ^= regC;
    }

    void out_()
    {
        output ~= to!string(getComboOperand() % 8);
    }

    void bdv()
    {
        int denominator = pow(2, getComboOperand());
        regB /= denominator;
    }

    void cdv()
    {
        int denominator = pow(2, getComboOperand());
        regC /= denominator;
    }

    string getOutputString() 
    {
        return output.join(",");
    }

    string getDebugString()
    {
        auto fmtString = 
"Register A: %d
Register B: %d
Register C: %d
Instruction PTR: %d";

        return format(
            fmtString, 
            regA, regB, regC, instPtr);
    }
}

// Parses a computer (according to the problem statement), 
// using the input file.
Computer readInput(string filename)
{
    auto file = File(filename);

    // Initial values for the registers.
    int regA, regB, regC = 0;
    int[] program = [];
    
    foreach (line; file.byLine())
    {
        auto splitLine = strip(line).split(": ");
        if (splitLine.length == 0) {
            continue;
        }
        switch (splitLine[0]) 
        {
            case "Register A": 
                regA = parse!int(splitLine[1]);
                break;

            case "Register B": 
                regB = parse!int(splitLine[1]);
                break;

            case "Register C": 
                regC = parse!int(splitLine[1]);
                break;

            case "Program":
                auto programRange = splitLine[1]
                    .split(",")
                    .map!(c => parse!int(c));

                foreach (n; programRange)
                {
                    program ~= n;
                }
                break;

            default:
                break;
        }
    }
    file.close();

    return new Computer(regA, regB, regC, program);
}

void main(string[] args)
{
    auto filename = args[1];
    auto computer = readInput(filename);

    writeln(computer.program);

    computer.runProgram();

    writeln(computer.getDebugString());
    writeln("Output: ", computer.getOutputString());
}
