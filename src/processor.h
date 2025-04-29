#ifndef PROCESSOR_H
#define PROCESSOR_H

#include <string>
#include <fstream>
#include "cache.h"
using namespace std;

class Bus;

class Processor {
private:
    int proc_id;
    Cache cache;
    ifstream trace_file;
    
    char current_op;
    uint32_t current_addr;
    bool has_instruction;
    
    bool is_next_to_miss;
    
    void read_next_instruction();
    bool process_instruction(Bus* bus, int global_cycle);
    
public:
    bool is_stalled;
    int stall_cycles;
    int total_cycles;
    int idle_cycles;

    int reads;
    int writes;
    
    Processor(int id, const string& trace_prefix, int s, int E, int b);
 
    
    char execute_cycle(Bus* bus, int global_cycle);
    char snoop_request(uint32_t address, bool is_write, int requesting_core, int& cycles);
    void invalidate_line(uint32_t address);
    
    int get_id() const { return proc_id; }
    Cache* get_cache() { return &cache; }
    
    int total_instructions() const { return reads + writes; }
};

#endif // PROCESSOR_H
