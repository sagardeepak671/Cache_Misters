#ifndef CACHE_H
#define CACHE_H

#include <vector>
#include <cstdint>
#include <climits>

class Bus;
struct CacheLine {
    char state = 'I';  // MESI states: 'M', 'E', 'S', 'I'
    uint32_t tag = 0;
    int last_used_cycle = 0;
};

class Cache {
private: 
    int set_bits;
    int num_sets;
    int associativity;
    int block_bits;
    int block_size;

    std::vector<std::vector<CacheLine>> cache_lines;

    int find_line(uint32_t set, uint32_t tag) const;
    int find_line_to_replace(uint32_t set) const;
    bool handle_write_back(uint32_t set, int way, int& stalls);

    bool access_read(uint32_t address, int& stalls, int core_id, Bus* bus, int global_cycle);
    bool access_write(uint32_t address, int& stalls, int core_id, Bus* bus, int global_cycle);

public:
    int misses;
    int tot_cycles;
    int hits;
    int evictions;
    int writebacks;
    int invalidations;
    int data_traffic;

    Cache(int s, int E, int b);
    
    bool access(uint32_t address, bool is_write, int& stalls, int core_id, Bus* bus, int global_cycle);
    char snoop(uint32_t address, bool is_write, int requesting_core, int& stalls, Bus* bus);
    
    char get_line_state(uint32_t address);
    void update_line_state(uint32_t address, char new_state);
    
    uint32_t get_set_index(uint32_t address) const;
    uint32_t get_tag(uint32_t address) const;
    uint32_t get_address_from_set_and_tag(uint32_t set, uint32_t tag) const;
    
    void force_update_line(uint32_t address, char state, int global_cycle);

    int getBlockSize() const { return block_size; }
};

#endif // CACHE_H
