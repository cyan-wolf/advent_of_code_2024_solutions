import java.io.File

// Representation of a claw machine from the problem statement.
data class Machine(val a: Pair<Int, Int>, val b: Pair<Int, Int>, val prize: Pair<Int, Int>)

// Parse an (x, y) pair corresponding to a button's displacement.
fun parseButtonPair(string: String): Pair<Int, Int> {
    val (xPart, yPart) = string.split(", ")

    val x = xPart.split("+")[1].toInt()
    val y = yPart.split("+")[1].toInt()

    return Pair(x, y)
}

// Parse an (x, y) pair corresponding to the prize location.
fun parsePrizePair(string: String): Pair<Int, Int> {
    val (xPart, yPart) = string.split(", ")

    val x = xPart.split("=")[1].toInt()
    val y = yPart.split("=")[1].toInt()

    return Pair(x, y)
}

// Adds a machine to the `machines` list by removing from `pairStack`.
fun gatherMachine(pairStack: ArrayDeque<Pair<Int, Int>>, machines: MutableList<Machine>) {
    val prize = pairStack.removeLast()
    val buttonB = pairStack.removeLast()
    val buttonA = pairStack.removeLast()

    machines.add(Machine(buttonA, buttonB, prize))
}

// Reads the list of machines from the input file.
fun readMachinesFromInput(): List<Machine> {
    val lines = File("input.txt").readLines()

    val machines = mutableListOf<Machine>()
    val pairStack = ArrayDeque<Pair<Int, Int>>()

    for (line in lines) {
        // Collect the current pairs in the stack into a machine
        // when a blank line is found.
        if (line.isBlank()) {
            gatherMachine(pairStack, machines);
            continue
        }

        val (label, pairString) = line.split(": ");

        val pair = when (label) {
            "Button A" -> parseButtonPair(pairString)
            "Button B" -> parseButtonPair(pairString)
            "Prize" -> parsePrizePair(pairString)
            else -> throw UnsupportedOperationException() // unreachable
        }
        pairStack.addLast(pair)
    }
    // Collect the remaining pairs into one last machine.
    gatherMachine(pairStack, machines)

    return machines
}

// Find the optimal configuration of A and B button presses
// for the given machine. Returns null if no such pair exists.
fun getFewestPressConfiguration(machine: Machine): Pair<Int, Int>? {
    for (bPresses in 0..100) {
        for (aPresses in 0..100) {

            val displacementA = Pair(aPresses * machine.a.first, aPresses * machine.a.second);
            val displacementB = Pair(bPresses * machine.b.first, bPresses * machine.b.second);

            val displacement = Pair(displacementA.first + displacementB.first,
                displacementA.second + displacementB.second)

            if (displacement == machine.prize) {
                return Pair(aPresses, bPresses)
            }
        }
    }

    return null
}

fun main() {
    val machines = readMachinesFromInput()

    var neededTokens = 0

    for (machine in machines) {
        val config = getFewestPressConfiguration(machine)

        if (config != null) {
            val (aPresses, bPresses) = config
            // Calculate the needed token amount.
            neededTokens += 3 * aPresses + bPresses
        }
    }
    println(neededTokens)
}