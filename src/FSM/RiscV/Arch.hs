{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

RISC-V architecture specification.
-}
module FSM.RiscV.Arch where

import Clash.Prelude
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

-- | RISC-V opcode.
data Opcode = OLoad   | OStore | OOp  | OOpImm
            | OBranch | OLui   | OJal | OJalr
            | OAuipc
    deriving (Eq, Show, Generic, NFDataX)
{-# ANN module (DataReprAnn 
                $(liftQ [t|Opcode|])
                7
                [ ConstrRepr 'OLoad    0x7f 0b0000011 [],
                  ConstrRepr 'OStore   0x7f 0b0100011 [],
                  ConstrRepr 'OOp      0x7f 0b0110011 [],
                  ConstrRepr 'OOpImm   0x7f 0b0010011 [],
                  ConstrRepr 'OBranch  0x7f 0b1100011 [],
                  ConstrRepr 'OLui     0x7f 0b0110111 [],
                  ConstrRepr 'OJal     0x7f 0b1101111 [],
                  ConstrRepr 'OJalr    0x7f 0b1100111 [],
                  ConstrRepr 'OAuipc   0x7f 0b0010111 []
                ]) #-}
deriveBitPack [t|Opcode|]

-- | RISC-V funct3 values for OP and OP-IMM instructions.
data Funct3Int = F3Add  -- ^ Addition.
               | F3Sll  -- ^ Shift left logical.
               | F3Slt  -- ^ Set if less than.
               | F3Sltu -- ^ Set if less than unsigned.
               | F3Xor  -- ^ XOR.
               | F3Srl  -- ^ Shift right logical.
               | F3Or   -- ^ OR.
               | F3And  -- ^ AND.
    deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | RISC-V alternate funct3 values for OP instructions.
data Funct3SInt = F3Sub -- ^ Subtraction.
                | F3Sra -- ^ Shift right arithmetic.
    deriving (Eq, Show, Generic, NFDataX)
{-# ANN module (DataReprAnn 
                $(liftQ [t|Funct3SInt|])
                3
                [ ConstrRepr 'F3Sub  0x7 0b000 [],
                  ConstrRepr 'F3Sra  0x7 0b101 []
                ]) #-}
deriveBitPack [t|Funct3SInt|]

-- | Width of the memory access.
data MemWidth = MWByte -- ^ Single byte.
              | MWHalf -- ^ Half word (two bytes).
              | MWWord -- ^ Word (four bytes).
    deriving (Eq, Show, Generic, NFDataX, BitPack, Enum)

-- | Signedness of the memory access.
data MemSign = MSSigned   -- ^ Signed value (sign-extend on load).
             | MSUnsigned -- ^ Unsigned value.
    deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | RISC-V funct3 values for LOAD and STORE instructions.
data Funct3Mem = Funct3Mem MemSign MemWidth
    deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | Branch comparison type.
data BranchType = BTEq  -- ^ Equality comparison.
                | BTLt  -- ^ Less-than comparison.
                | BTLtu -- ^ Unsigned less-than comparison.
    deriving (Eq, Show, Generic, NFDataX)
{-# ANN module (DataReprAnn 
                $(liftQ [t|BranchType|])
                2
                [ ConstrRepr 'BTEq   0x3 0b00 [],
                  ConstrRepr 'BTLt   0x3 0b10 [],
                  ConstrRepr 'BTLtu  0x3 0b11 []
                ]) #-}
deriveBitPack [t|BranchType|]

-- | Branch comparison negation.
data BranchNeg = BNPos -- ^ Branch if comparison true.
               | BNNeg -- ^ Branch if comparison false.
    deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | RISC-V funct3 values for BRANCH instructions.
data Funct3Branch = Funct3Branch BranchType BranchNeg
    deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | Funct3 field.
type Funct3 = BitVector 3

-- | Funct7 field.
type Funct7 = BitVector 7

-- | Register number.
type RegAddr = Index 32

-- | A CPU word.
type CpuWord = BitVector 32

-- | Signed CPU word.
type CpuSWord = Signed 32

-- | Unsigned CPU word.
type CpuUWord = Unsigned 32

-- | RISC-V instruction word split into bit fields.
data Instr = Instr {
    iFunct7 :: Funct7,
    iRS2 :: RegAddr,
    iRS1 :: RegAddr,
    iFunct3 :: Funct3,
    iRD :: RegAddr,
    iOpcode :: Opcode
} deriving (Eq, Show, Generic, NFDataX, BitPack)

-- | Decode a RISC-V instruction.
decodeInstr :: CpuWord -> Instr
decodeInstr = bitCoerce

