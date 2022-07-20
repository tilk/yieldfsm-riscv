{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

RISC-V ALU.
-}
module FSM.RiscV.Alu(
    AluInstr(..), AluIn(..), AluOut,
    emptyAluIn, doAluIn, aiAdd, alu, sigAlu, decodeAluInstr
) where

import Clash.Prelude
import FSM.RiscV.Arch

-- | ALU instruction.
data AluInstr = AIInt    Funct3Int    -- ^ Integer instruction.
              | AISInt   Funct3SInt   -- ^ Additional integer instruction.
              | AIBranch Funct3Branch -- ^ Branch instruction.
    deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | ALU input - ALU instruction and data inputs.
data AluIn = AluIn {
    aluInstr :: AluInstr, -- ^ ALU instruction.
    aluA :: CpuWord,      -- ^ ALU input A.
    aluB :: CpuWord       -- ^ ALU input B.
} deriving (Eq, Show, Generic, NFDataX)

-- | ALU output - a single word.
type AluOut = CpuWord

-- | Unspecified ALU input - for cycles where the ALU is idle.
emptyAluIn :: AluIn
emptyAluIn = AluIn undefined undefined undefined

-- | Specified ALU input.
doAluIn :: AluInstr -> CpuWord -> CpuWord -> AluIn
doAluIn = AluIn

-- | Addition instruction.
aiAdd :: AluInstr
aiAdd = AIInt F3Add

-- | The ALU.
alu :: AluInstr -> CpuWord -> CpuWord -> CpuWord
alu i wa wb = bitCoerce $ case i of
        AIInt F3Add -> a + b
        AIInt F3Sll -> a `shiftL` shiftAmt
        AIInt F3Slt -> processBranch BTLt BNPos
        AIInt F3Sltu -> processBranch BTLtu BNPos
        AIInt F3Xor -> a `xor` b
        AIInt F3Srl -> bitCoerce $ (bitCoerce a :: CpuUWord) `shiftR` shiftAmt
        AIInt F3Or -> a .|. b
        AIInt F3And -> a .&. b
        AISInt F3Sub -> a - b
        AISInt F3Sra -> a `shiftR` shiftAmt
        AIBranch (Funct3Branch bt x) -> processBranch bt x
    where
    a = bitCoerce wa :: CpuSWord
    b = bitCoerce wb :: CpuSWord
    shiftAmt = fromIntegral (truncateB $ bitCoerce b :: Unsigned 5) :: Int
    processBranch bt x = zeroExtend $ bitCoerce $ branchNegate x $ branchCompare bt :: CpuSWord
    branchCompare BTEq = a == b
    branchCompare BTLt = a < b
    branchCompare BTLtu = (bitCoerce a :: CpuUWord) < bitCoerce b
    branchNegate BNPos = id
    branchNegate BNNeg = not

-- | The ALU as a signal function.
sigAlu :: HiddenClockResetEnable dom
       => Signal dom AluIn
       -> Signal dom AluOut
sigAlu ai = (\(AluIn i wa wb) -> alu i wa wb) <$> ai

-- | Tests if this 'Funct3' corresponds to a shift instruction.
f3IsShift :: Funct3 -> Bool
f3IsShift (bitCoerce -> f3) = f3 == F3Sll || f3 == F3Srl

-- | Decodes an ALU instruction from the RISC-V opcode.
decodeAluInstr :: Opcode -> Funct3 -> Funct7 -> AluInstr
decodeAluInstr OOp f3 f7 | testBit f7 5 = AISInt (bitCoerce f3)
                         | otherwise = AIInt (bitCoerce f3)
decodeAluInstr OOpImm f3 f7 | f3IsShift f3 && testBit f7 5 = AISInt (bitCoerce f3)
                            | otherwise = AIInt (bitCoerce f3)
decodeAluInstr OBranch f3 _ = AIBranch (bitCoerce f3)
decodeAluInstr _ _ _ = undefined

