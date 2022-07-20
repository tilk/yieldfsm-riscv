{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Explicit RISC-V control path.
-}
module FSM.RiscV.ExplicitControl(
    explicitControl
) where

import Clash.Prelude
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.ExplicitData

-- | Control path automaton states.
data ExplicitState = ESFetch | ESDecode | ESExec | ESExecImm | ESAluWB
                   | ESMemAddr | ESMemRead | ESMemWrite | ESMemWB
                   | ESBranch | ESJal | ESJalr | ESLui
    deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | Control path automaton.
explicitControl :: HiddenClockResetEnable dom
                => Signal dom ExplicitStatus
                -> Signal dom ExplicitControl
explicitControl = mealy (\s i -> (transition s i, output s i)) ESFetch

-- | Control path automaton transition function.
transition :: ExplicitState -> ExplicitStatus -> ExplicitState
transition ESFetch st | esWbAck st = ESDecode
                      | otherwise  = ESFetch
transition ESDecode st = case esOpcode st of
    OLoad   -> ESMemAddr
    OStore  -> ESMemAddr
    OBranch -> ESBranch
    OJal    -> ESJal
    OJalr   -> ESJalr
    OOp     -> ESExec
    OOpImm  -> ESExecImm
    OLui    -> ESLui
    OAuipc  -> ESAluWB
transition ESMemAddr st = case esOpcode st of
    OLoad  -> ESMemRead
    OStore -> ESMemWrite
    _      -> undefined
transition ESMemRead st  | esWbAck st = ESMemWB
                         | otherwise  = ESMemRead
transition ESMemWrite st | esWbAck st = ESFetch
                         | otherwise  = ESMemWrite
transition ESExec    _  = ESAluWB
transition ESExecImm _  = ESAluWB
transition ESLui     _  = ESFetch
transition ESJal     _  = ESFetch
transition ESJalr    _  = ESFetch
transition ESAluWB   _  = ESFetch
transition ESMemWB   _  = ESFetch
transition ESBranch  _  = ESFetch

-- | Control path automaton output function.
output :: ExplicitState -> ExplicitStatus -> ExplicitControl
output s st = o s
    where
    alui = decodeAluInstr (esOpcode st) (esFunct3 st) (esFunct7 st)
    o ESFetch = defaultControl {
        ecAddrSel = Just AddrSelPC,
        ecIRWE = esWbAck st,
        ecAluCtl = Just $ AluControl aiAdd AluASelPC AluBSel4 }
    o ESDecode = defaultControl { 
        ecPCSel = Just PCSelAR,
        ecAluCtl = Just $ AluControl aiAdd AluASelPC AluBSelImm,
        ecRSWE = True }
    o ESExec = defaultControl {
        ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelRS2 }
    o ESExecImm = defaultControl {
        ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelImm }
    o ESLui = defaultControl {
        ecRDSel = Just RDSelImm }
    o ESAluWB = defaultControl {
        ecRDSel = Just RDSelAR }
    o ESMemAddr = defaultControl {
        ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }
    o ESMemRead = defaultControl {
        ecAddrSel = Just AddrSelAR,
        ecDRWE = esWbAck st }
    o ESMemWrite = defaultControl {
        ecAddrSel = Just AddrSelAR,
        ecWbWE = True }
    o ESMemWB = defaultControl {
        ecRDSel = Just RDSelDR }
    o ESBranch = defaultControl {
        ecPCSel = if esAluLSB st then Just PCSelAR else Nothing,
        ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelRS2 }
    o ESJal = defaultControl {
        ecPCSel = Just PCSelAR,
        ecRDSel = Just RDSelPC }
    o ESJalr = defaultControl {
        ecPCSel = Just PCSelAlu,
        ecRDSel = Just RDSelPC,
        ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }


