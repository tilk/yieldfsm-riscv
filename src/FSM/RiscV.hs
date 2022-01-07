module FSM.RiscV(
    module FSM.RiscV.Arch,
    module FSM.RiscV.Wishbone,
    rvcore,
    rvcoreExplicit
) where

import Clash.Prelude
import FSM
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.RegFile
import FSM.RiscV.Immediate
import FSM.RiscV.Wishbone
import FSM.RiscV.Explicit

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
fun useAlu (instr, a, b):
    output<alu> doAluIn instr a b
    ret ali
fun wbRead (adr, sel):
    do:
        yield<wb> readWishboneOut adr sel
    until wbAck wb'
    ret wbDatI wb'
fun wbWrite (adr, dat, sel):
    do:
        yield<wb> writeWishboneOut adr dat sel
    until wbAck wb'
fun wbReadF3 (f3, addr):
    let dat = call wbRead (mkAdr addr, mkSel f3 addr)
    ret mkDatI f3 addr dat
fun wbWriteF3 (f3, addr, val):
    ret call wbWrite (mkAdr addr, mkDatO f3 addr val, mkSel f3 addr)
forever:
    let pc4 = call useAlu (aiAdd, pc, 4)
    let iword = call wbRead (mkAdr pc, -1)
    let imm = immediate iword
    let instr = decodeInstr iword
    let aluInstr = decodeAluInstr (iOpcode instr) (iFunct3 instr) (iFunct7 instr)
    let pcimm = call useAlu (aiAdd, pc, imm)
    pc = pc4
    yield<rf> readRegFileIn (iRS1 instr) (iRS2 instr)
    case iOpcode instr
    | OLoad:
        let ares = call useAlu (aiAdd, rfRS1V rf, imm)
        yield
        let val = call wbReadF3 (bitCoerce $ iFunct3 instr, ares)
        yield <rf> writeRegFileIn (iRD instr) val
    | OStore:
        let ares = call useAlu (aiAdd, rfRS1V rf, imm)
        yield
        call wbWriteF3 (bitCoerce $ iFunct3 instr, ares, rfRS2V rf)
    | OOp:
        let ares = call useAlu (aluInstr, rfRS1V rf, rfRS2V rf)
        yield
        yield<rf> writeRegFileIn (iRD instr) ares
    | OOpImm:
        let ares = call useAlu (aluInstr, rfRS1V rf, imm)
        yield
        yield<rf> writeRegFileIn (iRD instr) ares
    | OBranch:
        let c = call useAlu (aluInstr, rfRS1V rf, rfRS2V rf)
        pc = if c == 0 then pc else pcimm
        yield
    | OLui:
        yield<rf> writeRegFileIn (iRD instr) imm
    | OJal:
        let ppc = pc
        pc = pcimm
        yield<rf> writeRegFileIn (iRD instr) ppc
    | OJalr:
        let ppc = pc
        pc = call useAlu (aiAdd, rfRS1V rf, imm)
        yield<rf> writeRegFileIn (iRD instr) ppc
    | OAuipc:
        yield<rf> writeRegFileIn (iRD instr) pcimm
|]

rvcore :: HiddenClockResetEnable dom
       => CpuWord
       -> Signal dom WishboneIn
       -> Signal dom WishboneOut
rvcore startPC wbi = wbo
    where
    (wbo, rfi, alui) = unbundle $ rvfsm startPC $ bundle (wbi, regFile rfi, sigAlu alui)

rvcoreExplicit :: HiddenClockResetEnable dom
               => CpuWord
               -> Signal dom WishboneIn
               -> Signal dom WishboneOut
rvcoreExplicit startPC wbi = wbo
    where
    (st, wbo) = explicitDatapath startPC (explicitControl st) wbi

