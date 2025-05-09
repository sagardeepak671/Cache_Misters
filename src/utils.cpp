#include "utils.h"
#include <iostream>
#include <string>
using namespace std;

void Wrong_Parameters(const char* exe) {
    cout<< "Usage: " << exe << " -t <trace_prefix> -s <s> -E <E> -b <b> [-o <outfile>] [-h]\n"
        << "Arguments:\n"
        << " -t <trace_prefix>: name of parallel application (e.g. app1) whose 4 traces are to be used\n"
        << " -s <s>: number of set index bits (number of sets in the cache = S = 2^s)\n"
        << " -E <E>: associativity (number of cache lines per set)\n"
        << " -b <b>: number of block bits (block size = B = 2^b)\n"
        << " -o <outfile>: logs output in file for plotting etc.\n"
        << " -h: prints this help\n";
}

uint32_t extract_bits(uint32_t address, int start, int count) {
    uint32_t mask = ((1 << count) - 1) << start;
    return (address & mask) >> start;
}
void print_simulation_parameters(ostream& out, const string& trace_prefix, int set_bits, int associativity, int block_bits, double cache_size_kb) {
    out << "Simulation Parameters:\n"
        << "Trace Prefix: " << trace_prefix << "\n"
        << "Set Index Bits: " << set_bits << "\n"<<"Associativity: " << associativity << "\n"
        << "Block Bits: " << block_bits << "\n"
        << "Block Size (Bytes): " << (1 << block_bits) << "\n"
        << "Number of Sets: " << (1 << set_bits) << "\n"
        << "Cache Size (KB per core): " << cache_size_kb << "\n"
        << "MESI Protocol: Enabled"<<"\n"
        << "Write Policy: Write-back, Write-allocate\n"
        << "Replacement Policy: LRU"<<"\n"
        << "Bus: Central snooping bus\n\n";
}
