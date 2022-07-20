{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

RISC-V cores.
-}
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

-- | RISC-V core implemented as single YieldFSM program.
rvcore :: HiddenClockResetEnable dom
       => CpuWord
       -> Signal dom WishboneIn
       -> Signal dom WishboneOut
rvcore startPC wbi = wbo
    where
    (wbo, rfi, alui) = unbundle $ rvfsm startPC $ bundle (wbi, regFile rfi, sigAlu alui)

-- | RISC-V core with YieldFSM control and an explicit data path.
rvcoreExplicitDP :: HiddenClockResetEnable dom
                 => CpuWord
               -> Signal dom WishboneIn
               -> Signal dom WishboneOut
rvcoreExplicitDP startPC wbi = wbo
    where
    (st, wbo) = explicitDatapath startPC (rvfsme st) wbi

-- | RISC-V core implemented in conventional way, with separate control and data paths.
rvcoreExplicit :: HiddenClockResetEnable dom
               => CpuWord
               -> Signal dom WishboneIn
               -> Signal dom WishboneOut
rvcoreExplicit startPC wbi = wbo
    where
    (st, wbo) = explicitDatapath startPC (explicitControl st) wbi

