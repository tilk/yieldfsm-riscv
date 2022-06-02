module FSM.RiscV.ExampleTop where

import Clash.Prelude
import Clash.Annotations.TH
import FSM.RiscV

startPC :: CpuWord
startPC = 0x400000

yieldfsm :: "clk" ::: Clock System
         -> "rst" ::: Reset System
         -> "en"  ::: Enable System
         -> "wb"  ::: Signal System WishboneIn
         -> "wb"  ::: Signal System WishboneOut
yieldfsm = exposeClockResetEnable $ rvcore startPC
makeTopEntity 'yieldfsm

yieldfsm_manual :: "clk" ::: Clock System
                -> "rst" ::: Reset System
                -> "en"  ::: Enable System
                -> "wb"  ::: Signal System WishboneIn
                -> "wb"  ::: Signal System WishboneOut
yieldfsm_manual = exposeClockResetEnable $ rvcoreManual startPC
makeTopEntity 'yieldfsm_manual

explicitdp :: "clk" ::: Clock System
           -> "rst" ::: Reset System
           -> "en"  ::: Enable System
           -> "wb"  ::: Signal System WishboneIn
           -> "wb"  ::: Signal System WishboneOut
explicitdp = exposeClockResetEnable $ rvcoreExplicitDP startPC
makeTopEntity 'explicitdp

explicit :: "clk" ::: Clock System
         -> "rst" ::: Reset System
         -> "en"  ::: Enable System
         -> "wb"  ::: Signal System WishboneIn
         -> "wb"  ::: Signal System WishboneOut
explicit = exposeClockResetEnable $ rvcoreExplicit startPC
makeTopEntity 'explicit


