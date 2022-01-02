module FSM.RiscV.ExampleTop where

import Clash.Prelude
import Clash.Annotations.TH
import FSM.RiscV

startPC :: CpuWord
startPC = 0x400000

topEntity :: "clk" ::: Clock System
          -> "rst" ::: Reset System
          -> "en"  ::: Enable System
          -> "wb"  ::: Signal System WishboneIn
          -> "wb"  ::: Signal System WishboneOut
topEntity = exposeClockResetEnable $ rvcore startPC
makeTopEntity 'topEntity

