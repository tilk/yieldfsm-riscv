module FSM.RiscV.YieldCtlData(
    rvfsm
) where

import Clash.Prelude
import FSM
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.RegFile
import FSM.RiscV.Immediate
import FSM.RiscV.Wishbone

[fsm|rvfsm :: HiddenClockResetEnable dom
           => CpuWord
           -> Signal dom (WishboneIn, RegFileOut, AluOut)
           -> Signal dom (WishboneOut, RegFileIn, AluIn)
param startPC
input (wb, rf, ali)
output wb = emptyWishboneOut
output rf = emptyRegFileIn
output alu = emptyAluIn
var pc = startPC
-- uses ALU in the current cycle
-- only single use per cycle possible!
fun useAlu (instr, a, b):
    output<alu> doAluIn instr a b
    ret ali
-- raw Wishbone read
fun wbRead (adr, sel):
    do:
        yield<wb> readWishboneOut adr sel
    until wbAck wb'
    ret wbDatI wb'
-- raw Wishbone write
fun wbWrite (adr, dat, sel):
    do:
        yield<wb> writeWishboneOut adr dat sel
    until wbAck wb'
-- Wishbone read, word width and sign encoded in f3 (funct3)
fun wbReadF3 (f3, addr):
    let dat = call wbRead (mkAdr addr, mkSel f3 addr)
    ret mkDatI f3 addr dat
-- Wishbone write, word width and sign encoded in f3 (funct3)
fun wbWriteF3 (f3, addr, val):
    ret call wbWrite (mkAdr addr, mkDatO f3 addr val, mkSel f3 addr)
-- main CPU loop
forever:
    let pc4 = call useAlu (aiAdd, pc, 4)
    let iword = call wbRead (mkAdr pc, -1)                          -- Fetch
    let imm = immediate iword
    let instr = decodeInstr iword
    let aluInstr = decodeAluInstr (iOpcode instr) (iFunct3 instr) (iFunct7 instr)
    let pcimm = call useAlu (aiAdd, pc, imm)
    pc = pc4
    yield<rf> readRegFileIn (iRS1 instr) (iRS2 instr)               -- Decode
    case iOpcode instr
    | OLoad:
        let ares = call useAlu (aiAdd, rfRS1V rf, imm)
        yield                                                       -- MemAddr
        let val = call wbReadF3 (bitCoerce $ iFunct3 instr, ares)   -- MemRead
        yield <rf> writeRegFileIn (iRD instr) val                   -- MemWB
    | OStore:
        let ares = call useAlu (aiAdd, rfRS1V rf, imm)
        yield                                                       -- MemAddr
        call wbWriteF3 (bitCoerce $ iFunct3 instr, ares, rfRS2V rf) -- MemWrite
    | OOp:
        let ares = call useAlu (aluInstr, rfRS1V rf, rfRS2V rf)
        yield                                                       -- Exec
        yield<rf> writeRegFileIn (iRD instr) ares                   -- AluWB
    | OOpImm:
        let ares = call useAlu (aluInstr, rfRS1V rf, imm)
        yield                                                       -- ExecImm
        yield<rf> writeRegFileIn (iRD instr) ares                   -- AluWB
    | OBranch:
        let c = call useAlu (aluInstr, rfRS1V rf, rfRS2V rf)
        pc = if c == 0 then pc else pcimm
        yield                                                       -- Branch
    | OLui:
        yield<rf> writeRegFileIn (iRD instr) imm                    -- Lui
    | OJal:
        let ppc = pc
        pc = pcimm
        yield<rf> writeRegFileIn (iRD instr) ppc                    -- Jal
    | OJalr:
        let ppc = pc
        pc = call useAlu (aiAdd, rfRS1V rf, imm)
        yield<rf> writeRegFileIn (iRD instr) ppc                    -- Jalr
    | OAuipc:
        yield<rf> writeRegFileIn (iRD instr) pcimm                  -- AluWB
|]

