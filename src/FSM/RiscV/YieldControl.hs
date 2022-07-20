{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

RISC-V core controller written in YieldFSM.
-}
module FSM.RiscV.YieldControl(
    rvfsme
) where

import Clash.Prelude
import FSM
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.ExplicitData

[fsm|rvfsme :: HiddenClockResetEnable dom
            => Signal dom ExplicitStatus
            -> Signal dom ExplicitControl
input st
forever:
    do:
        yield defaultControl {     {- Fetch -}
            ecAddrSel = Just AddrSelPC,
            ecIRWE = esWbAck st,
            ecAluCtl = Just $ AluControl aiAdd AluASelPC AluBSel4 }
    until esWbAck st'
    let alui = decodeAluInstr (esOpcode st) (esFunct3 st) (esFunct7 st)
    yield defaultControl {         {- Decode -}
        ecPCSel = Just PCSelAR,
        ecAluCtl = Just $ AluControl aiAdd AluASelPC AluBSelImm,
        ecRSWE = True }
    case esOpcode st
    | OLoad:
        yield defaultControl {     {- MemAddr -}
            ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }
        do:
            yield defaultControl { {- MemRead -}
                ecAddrSel = Just AddrSelAR,
                ecDRWE = esWbAck st }
        until esWbAck st'
        yield defaultControl {     {- MemWB -}
            ecRDSel = Just RDSelDR }
    | OStore:
        yield defaultControl {     {- MemAddr -}
            ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }
        do:
            yield defaultControl { {- MemWrite -}
                ecAddrSel = Just AddrSelAR,
                ecWbWE = True }
        until esWbAck st'
    | OOp:
        yield defaultControl {     {- Exec -}
            ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelRS2 }
        yield defaultControl {     {- AluWB -}
            ecRDSel = Just RDSelAR }
    | OOpImm:
        yield defaultControl {     {- ExecImm -}
            ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelImm }
        yield defaultControl {     {- AluWB -}
            ecRDSel = Just RDSelAR }
    | OBranch:
        yield defaultControl {     {- Branch -}
            ecPCSel = if esAluLSB st then Just PCSelAR else Nothing,
            ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelRS2 }
    | OLui:
        yield defaultControl {     {- Lui -}
            ecRDSel = Just RDSelImm }
    | OJal:
        yield defaultControl {     {- Jal -}
            ecPCSel = Just PCSelAR,
            ecRDSel = Just RDSelPC }
    | OJalr:
        yield defaultControl {     {- Jalr -}
            ecPCSel = Just PCSelAlu,
            ecRDSel = Just RDSelPC,
            ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }
    | OAuipc:
        yield defaultControl {     {- AluWB -}
            ecRDSel = Just RDSelAR }
|]

