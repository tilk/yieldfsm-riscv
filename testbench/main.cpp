#include "Vtoplevel.h"
#include "verilated.h"
#include <iostream>
#include <iomanip>
#include <memory>
#include <string>

int main(int argc, const char **argv, const char **env)
{
    Verilated::commandArgs(argc, argv);

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
            if (top->we_o && top->adr_o == 0xfffffff0) {
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

