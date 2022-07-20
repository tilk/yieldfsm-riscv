{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

RISC-V register file.
-}
module FSM.RiscV.RegFile where

import Clash.Prelude
import FSM.RiscV.Arch

-- | Register file inputs.
data RegFileIn = RegFileIn {
    rfRE  :: Bool,    -- ^ Read enable.
    rfWE  :: Bool,    -- ^ Write enable.
    rfRS1 :: RegAddr, -- ^ First read register selector.
    rfRS2 :: RegAddr, -- ^ Second read register selector.
    rfRD  :: RegAddr, -- ^ Write register selector.
    rfRDV :: CpuWord  -- ^ Write data.
} deriving (Eq, Show, Generic, NFDataX)

-- | Register file outputs.
data RegFileOut = RegFileOut {
    rfRS1V :: CpuWord, -- ^ First read data.
    rfRS2V :: CpuWord  -- ^ Second read data.
} deriving (Eq, Show, Generic, NFDataX)

-- | Register file inputs which don't change the register file state.
emptyRegFileIn :: RegFileIn
emptyRegFileIn = RegFileIn False False undefined undefined undefined undefined

-- | Register file inputs which order it to read two register values.
readRegFileIn :: RegAddr -> RegAddr -> RegFileIn
readRegFileIn rs1 rs2 = RegFileIn True False rs1 rs2 undefined undefined

-- | Register file inputs which order it to store new value to a register.
writeRegFileIn :: RegAddr -> CpuWord -> RegFileIn
writeRegFileIn rd rdv = RegFileIn False True undefined undefined rd rdv

-- | Register file definition.
regFile :: HiddenClockResetEnable dom => Signal dom RegFileIn -> Signal dom RegFileOut
regFile ri = regEn undefined (rfRE <$> ri) $ RegFileOut <$> (regVal (rfRS1 <$> ri)) <*> (regVal (rfRS2 <$> ri))
    where
    reg 0 = pure 0
    reg i = regEn 0 ((&&) <$> ((== i) . rfRD <$> ri) <*> (rfWE <$> ri)) (rfRDV <$> ri)
    storage = bundle $ map reg $ indices d32
    regVal i = (!!) <$> storage <*> i

