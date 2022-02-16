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
        yield defaultControl {
            ecAddrSel = Just AddrSelPC,
            ecIRWE = esWbAck st,
            ecAluCtl = Just $ AluControl aiAdd AluASelPC AluBSel4 }
    until esWbAck st'
    let alui = decodeAluInstr (esOpcode st) (esFunct3 st) (esFunct7 st)
    yield defaultControl {
        ecPCSel = Just PCSelAR,
        ecAluCtl = Just $ AluControl aiAdd AluASelPC AluBSelImm,
        ecRSWE = True }
    case esOpcode st
    | OLoad:
        yield defaultControl {
            ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }
        do:
            yield defaultControl {
                ecAddrSel = Just AddrSelAR,
                ecDRWE = esWbAck st }
        until esWbAck st'
        yield defaultControl {
            ecRDSel = Just RDSelDR }
    | OStore:
        yield defaultControl {
            ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }
        do:
            yield defaultControl {
                ecAddrSel = Just AddrSelAR,
                ecWbWE = True }
        until esWbAck st'
    | OOp:
        yield defaultControl {
            ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelRS2 }
        yield defaultControl {
            ecRDSel = Just RDSelAR }
    | OOpImm:
        yield defaultControl {
            ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelImm }
        yield defaultControl {
            ecRDSel = Just RDSelAR }
    | OBranch:
        yield defaultControl {
            ecPCSel = if esAluLSB st then Just PCSelAR else Nothing,
            ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelRS2 }
    | OLui:
        yield defaultControl {
            ecRDSel = Just RDSelImm }
    | OJal:
        yield defaultControl {
            ecPCSel = Just PCSelAR,
            ecRDSel = Just RDSelPC }
    | OJalr:
        yield defaultControl {
            ecPCSel = Just PCSelAlu,
            ecRDSel = Just RDSelPC,
            ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }
    | OAuipc:
        yield defaultControl {
            ecRDSel = Just RDSelAR }
|]

