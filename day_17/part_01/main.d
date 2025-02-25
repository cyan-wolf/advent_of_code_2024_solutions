import std.stdio;
import std.string;
import std.file;
import std.algorithm;
import std.conv;

// Represents a computer from the problem statement.
class Computer
{
    int regA, regB, regC;
    int[] program;

    int instPtr = 0;
    int[] output = [];

    this(int regA, int regB, int regC, int[] program) 
    {
        this.regA = regA;
        this.regB = regB;
        this.regC = regC;
        this.program = program;
    }

    void runProgram()
    {
        // TODO
    }

    void adv() 
    {
        // TODO
    }

    void bxl()
    {
        // TODO
    }
    
    void bst() 
    {
        // TODO
    }

    void jnz()
    {
        // TODO
    }

    void bxc()
    {
        // TODO
    }

    void out_()
    {
        // TODO
    }

    void bdv()
    {
        // TODO
    }

    void cdv()
    {
        // TODO
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
                    program = program ~ [n];
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

    writeln(computer.regA, " ", computer.regB, " ", computer.regC);
    writeln(computer.program);
}