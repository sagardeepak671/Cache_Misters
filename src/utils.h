#ifndef UTILS_H
#define UTILS_H

#include <iostream>
#include <string>
#include <cmath>
#include <cstdint>

// Print usage information
void Wrong_Parameters(const char* exe);
void print_simulation_parameters(std::ostream& out, const std::string& trace_prefix, int set_bits, int associativity, int block_bits, double cache_size_kb);
// Helper function to extract specific bits from an address
uint32_t extract_bits(uint32_t address, int start, int count);

#endif // UTILS_H
