#include "Vtoplevel.h"
#include "verilated.h"
#include <iostream>
#include <iomanip>
#include <memory>
#include <string>

int main(int argc, const char **argv, const char **env)
{
    Verilated::commandArgs(argc, argv);

    bool verbose = false;
    const char *str;
    str = Verilated::commandArgsPlusMatch("verbose");
    if (str && str[0]) verbose = true;

    std::unique_ptr<Vtoplevel> top(new Vtoplevel);

    top->rst = 0;
    top->en = 0;

    for (int time = 0; time < 100000; time++) {
        if (time > 9)
            top->rst = 0;
        else if (time > 4) {
            top->rst = 1;
            top->en = 1;
        }
        top->clk = time & 1;
        top->eval();
        if (top->cyc_o && top->stb_o) {
            if (verbose && top->clk && time > 8) {
                std::cout << std::hex << std::setfill('0')
                    << "adr_o=" << std::setw(8) << (top->adr_o << 2) << " "
                    << "sel_o=" << std::setw(1) << ((int)top->sel_o) << " "
                    << "dat_o=" << std::setw(8) << top->dat_o << " "
                    << "dat_i=" << std::setw(8) << top->dat_i << " "
                    << "we_o=" << (top->we_o ? "1" : "0") << " "
                    << "ack_i=" << (top->ack_i ? "1" : "0") << std::endl;
            }
            if (top->we_o && top->adr_o == 0xfffffff0 >> 2) {
                if (top->dat_o) {
                    std::cout << "PASS" << std::endl;
                    return 0;
                } else {
                    std::cout << "FAIL" << std::endl;
                    return -1;
                }
            }
        }
    }

    std::cout << "TIMEOUT" << std::endl;

    return -1;
}

