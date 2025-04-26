#include "bus.h"
#include "processor.h"

Bus::Bus() : total_transactions(0), total_traffic(0), free_time(0), 
message{-1, 0, 'I'} {
}

void Bus::add_processor(Processor* proc) {
    processors.push_back(proc);
}

char Bus::read(uint32_t address, int requesting_core, int& cycles, int current_cycle) {
    if (is_busy()) {
        return 'X'; // Bus is busy
    } 
    
    total_transactions++;
    char found_state = 'I';
    
    // Try to find the data in other caches
    for (Processor* proc : processors) {
        if (proc->get_id() != requesting_core) {
            int snoop_cycles = 0;
            char state = proc->snoop_request(address, false, requesting_core, snoop_cycles);
            cycles += snoop_cycles;
            
            if (state != 'I') {
                found_state = state;
                break; // Found in another cache
            }
        }
    } 
    return found_state;
}

// bool Bus::read_exclusive(uint32_t address, int requesting_core, int& cycles, int current_cycle) {
//     if (is_busy()) {
//         return false; // Bus is busy
//     }
    
//     total_transactions++;
//     bool found_in_other_cache = false;
    
//     // Try to find and invalidate in other caches
//     for (Processor* proc : processors) {
//         if (proc->get_id() != requesting_core) {
//             int snoop_cycles = 0;
//             char state = proc->snoop_request(address, true, requesting_core, snoop_cycles);
//             cycles += snoop_cycles;
            
//             if (state != 'I') {
//                 found_in_other_cache = true;
//                 // No break here - need to invalidate in all caches
//             }
//         }
//     }
    
//     // Set bus busy
//     if (found_in_other_cache) {
//         int block_size = processors[requesting_core]->get_cache()->getBlockSize();
//         int transfer_cycles = 2 * (block_size / 4); // 2 cycles per word
//         free_time = transfer_cycles;
//     } else {
//         // Memory access
//         free_time = 100;
//     }
    
//     return true;
// }

void Bus::invalidate(uint32_t address, int requesting_core, int& cycles, int current_cycle) {
    // if (is_busy()) {
    //     return; // Bus is busy
    // }
    total_transactions++;
    // Invalidate in other caches
    for (Processor* proc : processors) {
        if (proc->get_id() != requesting_core) { 
            proc->invalidate_line(address); 
        }
    }
}
