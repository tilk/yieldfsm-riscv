{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Top-level modules for defined RISC-V cores.
-}
module FSM.RiscV.ExampleTop where

import Clash.Prelude
import Clash.Annotations.TH
import FSM.RiscV

-- | Initial program counter.
startPC :: CpuWord
startPC = 0x400000

-- | RISC-V pure YieldFSM core - top-level module.
yieldfsm :: "clk" ::: Clock System
         -> "rst" ::: Reset System
         -> "en"  ::: Enable System
         -> "wb"  ::: Signal System WishboneIn
         -> "wb"  ::: Signal System WishboneOut
yieldfsm = exposeClockResetEnable $ rvcore startPC
makeTopEntity 'yieldfsm

-- | RISC-V YieldFSM control, explicit data path core - top-level module.
explicitdp :: "clk" ::: Clock System
           -> "rst" ::: Reset System
           -> "en"  ::: Enable System
           -> "wb"  ::: Signal System WishboneIn
           -> "wb"  ::: Signal System WishboneOut
explicitdp = exposeClockResetEnable $ rvcoreExplicitDP startPC
makeTopEntity 'explicitdp

-- | RISC-V explicit control and data path core - top-level module.
explicit :: "clk" ::: Clock System
         -> "rst" ::: Reset System
         -> "en"  ::: Enable System
         -> "wb"  ::: Signal System WishboneIn
         -> "wb"  ::: Signal System WishboneOut
explicit = exposeClockResetEnable $ rvcoreExplicit startPC
makeTopEntity 'explicit


