#include "processor.h"
#include "bus.h"
#include <iostream>
#include <sstream>
#include <algorithm>
using namespace std;


Processor::Processor(int id, const string& trace_prefix, int s, int E, int b)
    : proc_id(id), cache(s, E, b), has_instruction(false),is_stalled(false), stall_cycles(0),
      total_cycles(0), idle_cycles(0), reads(0), writes(0),has_new_instruction(false) {
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
 

ProcessorStatus Processor::execute_cycle(Bus* bus, int global_cycle) { 

    if (is_stalled) {
        // cout<<"stall cycle "<<stall_cycles<<endl;
        stall_cycles--;
        if(stall_cycles>=0)idle_cycles++;
        else {
            is_stalled = false;
        }
        return WaitingForBus;
    }
    if (has_instruction) {
        if(process_instruction(bus, global_cycle)){
            total_cycles++;
            read_next_instruction(); 
            return InstructionProcessed;  
        }else{
            if(has_new_instruction)total_cycles++;
            else {
                idle_cycles++;
            }
            has_new_instruction=false;
            return WaitingForBus;  
        }
    }
    return NoInstructionsLeft;
}

char Processor::snoop_request(uint32_t address, bool is_write, int requesting_core) {
    int stalls=0;
    char res = cache.snoop(address, is_write, requesting_core, stalls);
    stall_cycles+=stalls;
    if(stalls>0) is_stalled=true;
    return res;
}

void Processor::invalidate_line(uint32_t address) {
    // cout<<"clearline "<<address<<" "<<proc_id<<endl;
    cache.update_line_state(address, 'I');
}

void Processor::read_next_instruction() {
    string line;
    if (getline(trace_file, line)) {
        istringstream iss(line);
        iss >> current_op >> hex >> current_addr;
        has_instruction = true;
        has_new_instruction = true;
        if(current_op == 'R') reads++;
        else writes++;  
    }else{ 
        has_instruction = false; 
        has_new_instruction=false;
    } 
}

bool Processor::process_instruction(Bus* bus, int global_cycle) {
    bool is_write = (current_op == 'W');  
    int stalls = 0;
    bool success = cache.access(current_addr, is_write, stalls, proc_id, bus, global_cycle,has_new_instruction);
    if (stalls > 0) {
        is_stalled = true;
        stall_cycles = stalls;
        // cout<< "for core " << proc_id << " need " << stalls << " cycles" << endl;
    }
    return success;
} 