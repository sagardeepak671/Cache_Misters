#include "cache.h"
#include "bus.h"
#include <iostream>
#include <cmath>

Cache::Cache(int s, int E, int b)
    : set_bits(s), num_sets(1 << s), associativity(E), block_bits(b), block_size(1 << b),
      misses(0),tot_cycles(0), hits(0), evictions(0), writebacks(0), invalidations(0), data_traffic(0),
      cache_lines(num_sets, std::vector<CacheLine>(associativity, {'I', 0, 0})) {}

bool Cache::access(uint32_t address, bool is_write, int& stalls, int core_id, Bus* bus, int global_cycle,bool new_instruction) {
    return is_write ? access_write(address, stalls, core_id, bus, global_cycle,new_instruction): access_read(address, stalls, core_id, bus, global_cycle,new_instruction);
}

bool Cache::access_read(uint32_t address, int& stalls, int core_id, Bus* bus, int global_cycle,bool new_instruction) {
    uint32_t set = get_set_index(address);
    uint32_t tag = get_tag(address);
    int way = find_line(set, tag);
    // read hit
    if (way != -1 && cache_lines[set][way].state != 'I') { 
        cache_lines[set][way].last_used_cycle = global_cycle;  
        return true; // hit
    }

    // read miss 
    if (new_instruction) {
        cout<<"miss in core "<<core_id<<endl;
        misses++;
    }
    if (bus->is_busy()) return false;
    int replace_way = find_line_to_replace(set);
    if (replace_way != -1 && cache_lines[set][replace_way].state != 'I') {
        evictions++;
        if(cache_lines[set][replace_way].state == 'M') {
            uint32_t last_address = get_address_from_set_and_tag(set,cache_lines[set][replace_way].tag);
            handle_write_back(set, replace_way, stalls);
            bus->free_time = 100;
            stalls += 100;
            cache_lines[set][replace_way] = {'E', tag , global_cycle + stalls};
            // idle cycles--;
            return false;
        }
    }
    
    // Check other caches for the data (BusRd operation)
    char state_in_other_caches = bus->read(address,false, core_id, stalls, global_cycle);
    cout<<"checked for core"<<core_id<<" found "<<state_in_other_caches<<endl;
    // Determine new state based on whether data was found in other caches
    if (state_in_other_caches == 'I'){
        // Not in any other cache - get from memory in Exclusive state
        // stalls += 100; // Memory read takes 100 cycles 
        data_traffic += block_size;
        bus->free_time = 100;
        stalls += 100;
        cache_lines[set][replace_way] ={'E',get_tag(address),global_cycle + stalls};
    }else{
        // Found in another cache - get in Shared state
        //first cache to chache copy
        int words_in_block = block_size / 4;
        bus->free_time = 2 * words_in_block;
        stalls += (2 * words_in_block);
            cache_lines[set][replace_way] = {'S', get_tag(address), global_cycle + stalls};
        data_traffic += block_size;
        // write back in case for M
        if(state_in_other_caches == 'M'){
            // get that core id and cahnge the state to S
            stalls+=100;
            bus->free_time += 100;
            writebacks++;
        }
        // stalls += 2 * words_in_block; // 2 cycles per word for cache-to-cache transfer
    }
    return false;
}

bool Cache::access_write(uint32_t address, int& stalls, int core_id, Bus* bus, int global_cycle,bool new_instruction) {
    uint32_t set = get_set_index(address);
    uint32_t tag = get_tag(address); 
    int way = find_line(set, tag);

    // write hit
    if (way != -1 && cache_lines[set][way].state != 'I') {
        // hits++;
        cache_lines[set][way].last_used_cycle = global_cycle;
        
        if (cache_lines[set][way].state == 'M') {
            return true; // direct write already this address
        } else if (cache_lines[set][way].state == 'E') { 
            cache_lines[set][way].state = 'M';
            return true; // direct write already this address
        } else if (cache_lines[set][way].state == 'S') {
            if(bus->is_busy()) return false;
            // but count it as hit
            bus->invalidate(address, core_id, stalls, global_cycle);
            cache_lines[set][way].state = 'M';
            return true; // direct write already this address
        }
    }
    // write miss
    if (new_instruction) misses++;
    if (bus->is_busy()) return false;
    
    // Find line to replace if needed
    int replace_way = find_line_to_replace(set);
    uint32_t last_address = get_address_from_set_and_tag(set,cache_lines[set][replace_way].tag); 
    if (replace_way != -1 && cache_lines[set][replace_way].state != 'I') {
        evictions++;
        if(cache_lines[set][replace_way].state=='M'){
            handle_write_back(set, replace_way, stalls);
            stalls += 100;
            bus->free_time = 100; 
            cache_lines[set][replace_way] = {'I',last_address,0};
            return false; 
        }
    }

    // Check other caches for the data (BusRdX operation)
    char state_in_other_caches = bus->read(address,true, core_id, stalls, global_cycle);
    if (state_in_other_caches == 'M'){
        // write back in case for M  and setted to I;
        writebacks++;
        stalls += 100; // Memory write takes 100 cycles
        bus->free_time+=100;
    }
    // if(state_in_other_caches == 'X') {
    //     return false; // Bus is busy, retry in next cycle
    // }
    // no other copies read from memory
    data_traffic += block_size;
    stalls += 100; // Memory read takes 100 cycles
    bus->free_time = 100;
    cache_lines[set][replace_way]={'E',get_tag(address),global_cycle+stalls};
    //Read with intent to modify ... if other processors contain this address invalidate them
    bus->invalidate(address,core_id,stalls,global_cycle);
    return false;
}

char Cache::snoop(uint32_t address, bool is_write, int requesting_core, int& stalls) {
    uint32_t set = get_set_index(address);
    uint32_t tag = get_tag(address);
    int way = find_line(set, tag);
    if (way == -1 || cache_lines[set][way].state == 'I') return 'I';  // not in cache or invalid

    char current_state = cache_lines[set][way].state;
     
    if (is_write) {  
        if (current_state == 'M') {
            // Provide data and transition to Shared
            // handle_write_back(set, way, stalls);
            // uint32_t last_address = get_address_from_set_and_tag(set,way);
            // bus->free_time = 100;
            // bus->message = {requesting_core, last_address, 'E'};
            cache_lines[set][way].state = 'I';
            return 'M';
        }
    } else { // BusRd
        if (current_state == 'M') {
            // Provide data and transition to Shared
            // handle_write_back(set, way, stalls);
            // uint32_t last_address = get_address_from_set_and_tag(set,way);
            // bus->free_time = 100;
            // bus->message = {requesting_core, last_address, 'E'};
            cache_lines[set][way].state = 'S'; 
            return 'M';
        } else if (current_state == 'E') {
            // Provide data and transition to Shared
            cache_lines[set][way].state = 'S';
            return 'E';
        } else if (current_state == 'S') {
            return 'S';
        }
        stalls +=( 2 * block_size / 4 );
    } 
    return current_state;
}

// HELPER FUNCTIONS
int Cache::find_line(uint32_t set, uint32_t tag) const {
    for (int i = 0; i < associativity; i++)
        if (cache_lines[set][i].tag == tag && cache_lines[set][i].state != 'I') return i;
    return -1;
}

int Cache::find_line_to_replace(uint32_t set) const {
    int lru = 0, min_cycle = INT_MAX;
    for (int i = 0; i < associativity; i++) {
        if (cache_lines[set][i].state == 'I') return i;
        if (cache_lines[set][i].last_used_cycle < min_cycle) {
            min_cycle = cache_lines[set][i].last_used_cycle;
            lru = i;
        }
    }
    return lru;
}

uint32_t Cache::get_address_from_set_and_tag(uint32_t set, uint32_t tag) const {
    return (tag << (block_bits + set_bits)) | (set << block_bits);
}

bool Cache::handle_write_back(uint32_t set, int way, int& stalls) {
    if (cache_lines[set][way].state == 'M') {
        writebacks++;
        // stalls += 100; // Memory write takes 100 cycles as per specification
        data_traffic += block_size;
        return true;
    }
    return false;
}

uint32_t Cache::get_set_index(uint32_t address) const {
    return (address >> block_bits) & (num_sets - 1);
}

uint32_t Cache::get_tag(uint32_t address) const {
    return address >> (block_bits + set_bits);
}

char Cache::get_line_state(uint32_t address) {
    uint32_t set = get_set_index(address);
    uint32_t tag = get_tag(address);
    int way = find_line(set, tag);
    if (way != -1) return cache_lines[set][way].state;
    return 'I';
}

void Cache::update_line_state(uint32_t address, char new_state) {
    uint32_t set = get_set_index(address);
    uint32_t tag = get_tag(address);
    int way = find_line(set, tag);
    if (way != -1) cache_lines[set][way].state = new_state;   
}

void Cache::force_update_line(uint32_t address, char state, int global_cycle) {
    auto& line = cache_lines[get_set_index(address)][find_line_to_replace(get_set_index(address))];
    line = {state,get_tag(address),global_cycle};
}