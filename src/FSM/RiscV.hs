module FSM.RiscV(
    module FSM.RiscV.Arch,
    module FSM.RiscV.Wishbone,
    rvcore,
    rvcoreExplicitDP,
    rvcoreExplicit
) where

import Clash.Prelude
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.RegFile
import FSM.RiscV.Wishbone
import FSM.RiscV.ExplicitControl
import FSM.RiscV.ExplicitData
import FSM.RiscV.YieldControl
import FSM.RiscV.YieldCtlData

rvcore :: HiddenClockResetEnable dom
       => CpuWord
       -> Signal dom WishboneIn
       -> Signal dom WishboneOut
rvcore startPC wbi = wbo
    where
    (wbo, rfi, alui) = unbundle $ rvfsm startPC $ bundle (wbi, regFile rfi, sigAlu alui)

rvcoreExplicitDP :: HiddenClockResetEnable dom
                 => CpuWord
               -> Signal dom WishboneIn
               -> Signal dom WishboneOut
rvcoreExplicitDP startPC wbi = wbo
    where
    (st, wbo) = explicitDatapath startPC (rvfsme st) wbi

rvcoreExplicit :: HiddenClockResetEnable dom
               => CpuWord
               -> Signal dom WishboneIn
               -> Signal dom WishboneOut
rvcoreExplicit startPC wbi = wbo
    where
    (st, wbo) = explicitDatapath startPC (explicitControl st) wbi

