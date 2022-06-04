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

The source is in the `src` directory. Other directories contain:

* `riscv-tests` -- [unit tests for the RISC-V architecture](https://github.com/riscv/riscv-tests). Building requires installing the RISC-V toolchain (in Debian: `gcc-riscv64-unknown-elf`), also `srecord` is needed to prepare binary images. Building the unit tests is required before running the testbench.
* `testbench` -- the testbench for RISC-V cores. It contains three subdirectories for each of the implementations. Building and running the testbench requires installing `verilator`.
* `synth` -- synthesis benchmarks. Like `testbench`, contains subdirectories for each of the implementations. Running the synthesis benchmarks requires `yosys` and `nextpnr-ice40`.

## Building and testing 

Building the cores requires `stack` (use `stack build`).

To test the cores, one first has to build (using `make`) the tests (`riscv-tests`), and then compile and run the testbench (`testbench`).

To run synthesis benchmarks, run `make` in the `synth` subdirectory, then run `extract_perf.pl` (requires Perl) to extract performance numbers (number of used LCs and maximum frequency Fmax) to JSON files `lc.json` and `fmax.json`.
