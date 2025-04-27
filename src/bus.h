#ifndef BUS_H
#define BUS_H

#include <vector>
#include <cstdint>

class Processor;

struct Message {  // message for read from ram and write back to ram or message between caches
    int core_id;
    uint32_t address ;
    char new_state;
};

 

class Bus {
private:
    std::vector<Processor*> processors;

public:
    int total_transactions;
    int total_traffic;
    int free_time;

    Message message; 

    Bus();

    void add_processor(Processor* proc);
    char read(uint32_t address,bool is_write, int requesting_core, int& cycles, int current_cycle);
    bool read_exclusive(uint32_t address, int requesting_core, int& cycles, int current_cycle);
    void invalidate(uint32_t address, int requesting_core, int& cycles, int current_cycle);

    bool is_busy() const { return free_time > 0; }
};

#endif // BUS_H
