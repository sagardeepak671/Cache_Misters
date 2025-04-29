#include "processor.h"
#include "bus.h"
#include <iostream>
#include <sstream>
#include <algorithm>
using namespace std;

Processor::Processor(int id, const string& trace_prefix, int s, int E, int b)
    : proc_id(id), cache(s, E, b), has_instruction(false), is_next_to_miss(false), stall_cycles(0),
      total_cycles(0), idle_cycles(0), reads(0), writes(0) {
    // Open trace file
    string filename = trace_prefix + "_proc" + to_string(id) + ".trace";
    trace_file.open(filename);
    if (!trace_file.is_open()) {
        cerr << "Error: Could not open trace file " << filename << endl;
        exit(1);
    }
    // Read first instruction
    read_next_instruction();
}
 

bool Processor::execute_cycle(Bus* bus, int global_cycle) { 
    if (has_instruction) {
        process_instruction(bus, global_cycle);
        total_cycles++; 
        return true;
    }
    return false; // No more instructions
}

char Processor::snoop_request(uint32_t address, bool is_write, int requesting_core, int& cycles) {
    return cache.snoop(address, is_write, requesting_core, cycles, nullptr);
}

void Processor::invalidate_line(uint32_t address) {
    cache.update_line_state(address, 'I');
    cache.invalidations++;
}

void Processor::read_next_instruction() {
    string line;
    if (getline(trace_file, line)) {
        istringstream iss(line);
        iss >> current_op >> hex >> current_addr;
        has_instruction = true;
        if(current_op == 'R') reads++;
        else writes++;  
    }else{ 
        has_instruction = false; 
    }
}

void Processor::process_instruction(Bus* bus, int global_cycle) {
    bool is_write = (current_op == 'W');  
    int stalls = 0;
    bool success = cache.access(current_addr, is_write, stalls, proc_id, bus, global_cycle);
    if (!success) {
        idle_cycles++;  
        if(is_next_to_miss) {
            cache.misses++;   // misses in cache
            is_next_to_miss = false; 
        }
    }
    else{
        read_next_instruction();
        is_next_to_miss = true;
    } 
} 