#include "simulate.h"
#include "utils.h"
#include <iostream>
#include <iomanip>
#include <algorithm>
using namespace std;
Simulator::Simulator(const string& trace_prefix, int s, int E, int b)
    : trace_prefix(trace_prefix), set_bits(s), associativity(E), block_bits(b) {
    // Create processors
    for (int i = 0; i < 4; i++) {
        processors.push_back(new Processor(i, trace_prefix, s, E, b));
        bus.add_processor(processors[i]);
    }
} 
Simulator::~Simulator() {
    for (auto proc : processors) {
        delete proc;
    }
}

void Simulator::run() {
    bool running = true;
    int global_cycle = 1;
    bool worked = false;
    int start_processor = 0;
    while (running) {
        running = false;
        worked = false;
        // Execute one cycle for each processor
        cout<<"Cycle: "<<global_cycle<<endl;
        start_processor = global_cycle % 4;
        for(int i=0;i<4;i++){
            ProcessorStatus res = processors[start_processor]->execute_cycle(&bus,global_cycle);
            if(res == WaitingForBus || res == InstructionProcessed){
                running = true;
            }
            if(res==InstructionProcessed){
                worked=true;
            }
            start_processor = (start_processor + 1) % 4;
        }
        // for (Processor* proc : processors) {
        //     ProcessorStatus res = proc->execute_cycle(&bus,global_cycle);
        //     if(res == WaitingForBus || res == InstructionProcessed){
        //         running = true;
        //     }
        //     if(res==InstructionProcessed){
        //         worked=true;
        //     }
        //     // cout<<"*********"<<endl;
        //     // cout<< "Core " << proc->get_id() << " status: " << res << endl;
        //     // cout<<"idle cycles: "<<proc->idle_cycles<<endl;
        //     // cout<<"total cycles: "<<proc->total_cycles<<endl;
        // }
        // cout<<"-------------------"<<endl;
        if(worked==false){
            // do free the bus instantly adding up it to global cycles
            global_cycle+=bus.free_time;
            for(Processor* proc:processors){
                if(proc->get_has_instruction()){
                    proc->idle_cycles += bus.free_time;
                    proc->stall_cycles=0;
                    proc->is_stalled=false;
                }
            }
            bus.free_time=0;
        }
        bus.free_time = max(0, bus.free_time - 1);
        if(bus.free_time ==0){
            // execute the message
            for(Processor* proc: processors){ 
                if(proc->get_id() == bus.message.core_id){
                    proc->get_cache()->force_update_line(bus.message.address,bus.message.new_state,global_cycle);
                    bus.message = {-1, 0, 'I'};
                }
            }
        }
        global_cycle++;
    }
}

void Simulator::print_results(ostream& out) {
    double cache_size_kb = (1 << set_bits) * associativity * (1 << block_bits) / 1024.0;
    print_simulation_parameters(out, trace_prefix, set_bits, associativity, block_bits, cache_size_kb);
    for (int i = 0; i < processors.size(); i++) {
        Processor* proc = processors[i];
        Cache* cache = proc->get_cache(); 
        double miss_rate = proc->total_instructions() > 0 ? 100.0 * cache->misses / proc->total_instructions() : 0.0;
        out << "Core " << i << " Statistics:\n"
            << "Total Instructions: " << proc->total_instructions() << "\n"
            << "Total Reads: " << proc->reads << "\n"
            << "Total Writes: " << proc->writes << "\n"
            << "Total Execution Cycles: " << proc->total_cycles << "\n"
            << "Idle Cycles: " << proc->idle_cycles << "\n"
            << "Cache Misses: " << cache->misses << "\n"
            << "Cache Miss Rate: " << fixed << setprecision(2) << miss_rate << "%\n"
            << "Cache Evictions: " << cache->evictions << "\n"
            << "Writebacks: " << cache->writebacks << "\n"
            << "Bus Invalidations: " << cache->invalidations << "\n"
            << "Data Traffic (Bytes): " << cache->data_traffic << "\n\n";
    }
    out << "Overall Bus Summary:\n"
        << "Total Bus Transactions: " << bus.total_transactions << "\n"
        << "Total Bus Traffic (Bytes): " << bus.total_traffic << "\n";
}
