module FSM.RiscV.RegFile where

import Clash.Prelude
import FSM.RiscV.Arch

data RegFileIn = RegFileIn {
    rfRS1 :: RegAddr,
    rfRS2 :: RegAddr,
    rfRD  :: RegAddr,
    rfRDV :: CpuWord,   
    rfWE  :: Bool
} deriving (Eq, Show, Generic, NFDataX)

data RegFileOut = RegFileOut {
    rfRS1V :: CpuWord,
    rfRS2V :: CpuWord
} deriving (Eq, Show, Generic, NFDataX)

regFile :: HiddenClockResetEnable dom => Signal dom RegFileIn -> Signal dom RegFileOut
regFile ri = RegFileOut <$> (regVal (rfRS1 <$> ri)) <*> (regVal (rfRS2 <$> ri))
    where
    reg 0 = pure 0
    reg i = regEn 0 ((&&) <$> ((== i) . rfRD <$> ri) <*> (rfWE <$> ri)) (rfRDV <$> ri)
    storage = bundle $ map reg $ indices d32
    regVal i = (!!) <$> storage <*> i

