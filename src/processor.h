#ifndef PROCESSOR_H
#define PROCESSOR_H

#include <string>
#include <fstream>
#include "cache.h"
using namespace std;

class Bus;

enum ProcessorStatus{
    WaitingForBus,
    InstructionProcessed,
    NoInstructionsLeft
};

class Processor {
private:
    int proc_id;
    ifstream trace_file;
    
    char current_op;
    uint32_t current_addr;
    bool has_instruction;
    
    bool has_new_instruction;
    
    void read_next_instruction();
    bool process_instruction(Bus* bus, int global_cycle);
    
public:
    Cache cache;
    bool is_stalled;
    int stall_cycles;
    int total_cycles;
    int idle_cycles;

    int reads;
    int writes;
    
    Processor(int id, const string& trace_prefix, int s, int E, int b);
 
    
    ProcessorStatus execute_cycle(Bus* bus, int global_cycle);
    char snoop_request(uint32_t address, bool is_write, int requesting_core);
    void invalidate_line(uint32_t address);
    
    int get_id() const { return proc_id; }
    Cache* get_cache() { return &cache; }
    
    int total_instructions() const { return reads + writes; }

    bool get_has_instruction() const { return has_instruction; }
};

#endif // PROCESSOR_H
