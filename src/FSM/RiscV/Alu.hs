module FSM.RiscV.Alu where

import Clash.Prelude
import FSM.RiscV.Arch

data AluInstr = AIInt Funct3Int | AISInt Funct3SInt | AIBranch Funct3Branch
    deriving (Eq, Show, Generic, NFDataX, BitPack)

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
