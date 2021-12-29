module FSM.RiscV.RegFile where

import Clash.Prelude
import FSM.RiscV.Arch

data RegFileIn = RegFileIn {
    rfRE  :: Bool,
    rfWE  :: Bool,
    rfRS1 :: RegAddr,
    rfRS2 :: RegAddr,
    rfRD  :: RegAddr,
    rfRDV :: CpuWord
} deriving (Eq, Show, Generic, NFDataX)

data RegFileOut = RegFileOut {
    rfRS1V :: CpuWord,
    rfRS2V :: CpuWord
} deriving (Eq, Show, Generic, NFDataX)

emptyRegFileIn :: RegFileIn
emptyRegFileIn = RegFileIn False False undefined undefined undefined undefined

readRegFileIn :: RegAddr -> RegAddr -> RegFileIn
readRegFileIn rs1 rs2 = RegFileIn True False rs1 rs2 undefined undefined

writeRegFileIn :: RegAddr -> CpuWord -> RegFileIn
writeRegFileIn rd rdv = RegFileIn False True undefined undefined rd rdv

regFile :: HiddenClockResetEnable dom => Signal dom RegFileIn -> Signal dom RegFileOut
regFile ri = regEn undefined (rfRE <$> ri) $ RegFileOut <$> (regVal (rfRS1 <$> ri)) <*> (regVal (rfRS2 <$> ri))
    where
    reg 0 = pure 0
    reg i = regEn 0 ((&&) <$> ((== i) . rfRD <$> ri) <*> (rfWE <$> ri)) (rfRDV <$> ri)
    storage = bundle $ map reg $ indices d32
    regVal i = (!!) <$> storage <*> i

