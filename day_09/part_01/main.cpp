#include <iostream>
#include <fstream>
#include <string>
#include <vector>

// Reads the disk using blocks computed from the input file.
std::vector<int> read_disk() {
    std::string line;

    std::ifstream file("input.txt");
    std::getline(file, line);

    std::vector<int> disk;

    for (int i = 0; i < line.length(); ++i) {
        int size = line[i] - '0';
        
        // `-1` represents an empty space.
        int block = -1;

        // A non empty space.
        if (i % 2 == 0) {
            // Calculate the ID based on the index `i`.
            int id = i / 2;
            block = id;
        }

        // Fill the disk a `size` amount of times with `block`.
        for (int _j = 0; _j < size; ++_j) {
            disk.push_back(block);
        }
    }
    return disk;
}

// Move all the free space to the right side of the disk.
void adjust_disk(std::vector<int> &disk) {
    int front_idx = 0;
    int back_idx = disk.size() - 1;

    while (front_idx < back_idx) {
        // If the front index is not on a free space, 
        // then move it.
        if (disk[front_idx] != -1) {
            front_idx++;
            continue;
        }
        // If the back index is not on a block, 
        // then move it.
        if (disk[back_idx] == -1) {
            back_idx--;
            continue;
        }
        // Swap the contents of the front and back indices.
        // Note: The front index should always be on a free space before swaping.
        disk[front_idx] = disk[back_idx];
        disk[back_idx] = -1;

        front_idx++;
        back_idx--;
    }
}

// Calculates the checksum.
// Note: `long long` is needed to avoid integer overflow.
long long calc_checksum(const std::vector<int> &disk) {
    long long sum = 0;

    for (long long i = 0; i < disk.size(); ++i) {
        if (disk[i] == -1) {
            break;
        }
        sum += i * disk[i];
    }
    return sum;
}

int main() {
    std::vector<int> disk = read_disk();

    adjust_disk(disk);

    std::cout << calc_checksum(disk) << std::endl;

    return 0;
}

