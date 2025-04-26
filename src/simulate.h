#ifndef SIMULATE_H
#define SIMULATE_H

#include <string>
#include <vector>
#include <ostream>
#include "processor.h"
#include "bus.h"
using namespace std;

class Simulator {
private:
    string trace_prefix;
    int set_bits;
    int associativity;
    int block_bits;
    
    vector<Processor*> processors;
    Bus bus;

public:
    Simulator(const string& trace_prefix, int s, int E, int b); 
    ~Simulator();
    void run();
    void print_results(ostream& out);
};

#endif // SIMULATE_H
