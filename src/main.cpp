#include <iostream>
#include <fstream>
#include <getopt.h>
#include "simulate.h"
#include "utils.h"

int main(int argc, char* argv[]) {
    std::string trace_prefix, outfile;
    int s = -1, E = -1, b = -1, opt;
    
    while ((opt = getopt(argc, argv, "t:s:E:b:o:h")) != -1) {
        if (opt == 't') trace_prefix = optarg;
        else if (opt == 's') s = std::stoi(optarg);
        else if (opt == 'E') E = std::stoi(optarg);
        else if (opt == 'b') b = std::stoi(optarg);
        else if (opt == 'o') outfile = optarg;
        else { Wrong_Parameters(argv[0]); return opt == 'h' ? 0 : 1; }
    }
    
    if (trace_prefix.empty() || s < 0 || E <= 0 || b < 0) {
        Wrong_Parameters(argv[0]);
        return 1;
    }
    
    try {
        Simulator simulator(trace_prefix, s, E, b);
        simulator.run();
        
        if (!outfile.empty()) {
            std::ofstream out(outfile);
            if (out) simulator.print_results(out);
            else { std::cerr << "Error: Could not open output file " << outfile << std::endl; return 1; }
        } else simulator.print_results(std::cout);
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
