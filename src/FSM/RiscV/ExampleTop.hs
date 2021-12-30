module FSM.RiscV.ExampleTop where

import Clash.Prelude
import Clash.Annotations.TH
import FSM.RiscV

startPC :: CpuWord
startPC = 0x400000

topEntity :: "CLK" ::: Clock System
          -> "RST" ::: Reset System
          -> "EN"  ::: Enable System
          -> "WB"  ::: Signal System WishboneIn
          -> "WB"  ::: Signal System WishboneOut
topEntity = exposeClockResetEnable $ rvcore startPC
makeTopEntity 'topEntity

