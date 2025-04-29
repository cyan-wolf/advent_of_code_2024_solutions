#include <iostream>
#include <string>
#include <fstream>
#include <unordered_map>
#include <vector>
#include <set>
#include <algorithm>

using direct_connection_map = std::unordered_map<std::string, std::vector<std::string>>;
using triplet = std::set<std::string>;

// Reads a map from a computer name to a vector of its direct connections.
direct_connection_map get_input(std::string filename) {
    std::ifstream input(filename);

    direct_connection_map connections;

    for (std::string line; std::getline(input, line);) {
        auto first = line.substr(0, 2);
        auto second = line.substr(3, 2);
        
        // Add a new connection.
        if (connections.find(first) == connections.end()) {
            connections.insert({first, {}});
        }
        connections.at(first).push_back(second);

        // Add the connection in the other direction.
        if (connections.find(second) == connections.end()) {
            connections.insert({second, {}});
        }
        connections.at(second).push_back(first);
    }
    return connections;
}

// Gathers all the connected triplets involving the 
// given key. Uses the direct connections map for 
// checking if two computers are directly connected.
// All valid triplets are collected onto the `all_triplets` set.
void gather_all_connected_triplets(
    std::string key, 
    const direct_connection_map &connections,
    std::set<triplet> &all_triplets) {

    auto direct_conns = connections.at(key);

    // Iterate over all pairs in the vector of direction connections once.
    for (int i = 0; i < direct_conns.size() - 1; i++) {
        for (int j = i + 1; j < direct_conns.size(); j++) {
            auto conn1 = direct_conns.at(i);
            auto conn2 = direct_conns.at(j);

            // If the selected connections are directly connected, then the 
            // triplet is valid.
            auto conn1_dir_conns = connections.at(conn1);
            if (std::find(conn1_dir_conns.begin(), conn1_dir_conns.end(), conn2) != conn1_dir_conns.end()) {
                // Gather the valid triplet.
                all_triplets.insert({key, conn1, conn2});
            }
        }
    }
}

// Entry point for the program.
// The input file path should be given as a command-line argument.
int main(int argc, char *argv[]) {
    auto filename = argv[1];
    auto connections = get_input(filename);

    std::set<triplet> all_triplets;

    // Iterate over all computers.
    for (auto it = connections.begin(); it != connections.end(); it++) {
        auto key = it->first;

        // Only consider computers with a name that starts 
        // with 't'.
        if (key.at(0) == 't') {
            // Gather the valid triplets that contain the computer given by `key`.
            gather_all_connected_triplets(key, connections, all_triplets);
        }
    }
    std::cout << all_triplets.size() << std::endl;
}
