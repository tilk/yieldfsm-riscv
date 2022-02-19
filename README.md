# yieldfsm-riscv

[YieldFSM](https://github.com/tilk/yieldfsm) is a proof of concept DSL for [Clash](https://clash-lang.org/) designed for specifying (Mealy)
finite state machines using imperative, procedural code.
This project implements a proof-of-concept multicycle RV32I CPU using Clash and YieldFSM.
The processor has a Wishbone Classic interface, allowing easy interfacing to memory and peripherals.

## Structure

Three implementations are provided, in the file `ExampleTop.hs`:

* `explicit` -- traditional, explicit, Clash-only implementation (`ExplicitControl.hs` and `ExplicitData.hs`);
* `explicitdp` -- explicit data path, controller written using YieldFSM (`YieldControl.hs` and `ExplicitData.hs`);
* `yieldfsm` -- combined control and data path in single YieldFSM source (`YieldCtlData.hs`).

## Testing 

The core is tested using [official unit tests](https://github.com/riscv/riscv-tests).
The tests are in the `riscv-tests` directory, building them requires installing the RISC-V toolchain (in Debian: gcc-riscv64-unknown-elf).

The test runner is in the `testbench` directory. To run tests, Verilator is required (in Debian: verilator).

