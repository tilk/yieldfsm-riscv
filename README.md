# yieldfsm-riscv

[YieldFSM](https://github.com/tilk/yieldfsm) is a proof of concept DSL for [Clash](https://clash-lang.org/) designed for specifying (Mealy)
finite state machines using imperative, procedural code.
This project implements a proof-of-concept multicycle RV32I CPU using Clash and YieldFSM.
The processor has a Wishbone Classic interface, allowing easy interfacing to memory and peripherals.

## Testing 

The core is tested using [official unit tests](https://github.com/riscv/riscv-tests).

