module FSM.RiscV.Arch where

import Clash.Prelude
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

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

data Funct3Int = F3Add | F3Sll | F3Slt | F3Sltu
               | F3Xor | F3Srl | F3Or  | F3And
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data Funct3SInt = F3Sub | F3Sra
    deriving (Eq, Show, Generic, NFDataX)
{-# ANN module (DataReprAnn 
                $(liftQ [t|Funct3SInt|])
                3
                [ ConstrRepr 'F3Sub  0x7 0b000 [],
                  ConstrRepr 'F3Sra  0x7 0b101 []
                ]) #-}
deriveBitPack [t|Funct3SInt|]

data MemWidth = MWByte | MWHalf | MWWord
    deriving (Eq, Show, Generic, NFDataX, BitPack, Enum)

data MemSign = MSSigned | MSUnsigned
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data Funct3Mem = Funct3Mem MemSign MemWidth
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data BranchType = BTEq | BTLt | BTLtu
    deriving (Eq, Show, Generic, NFDataX)
{-# ANN module (DataReprAnn 
                $(liftQ [t|BranchType|])
                2
                [ ConstrRepr 'BTEq   0x3 0b00 [],
                  ConstrRepr 'BTLt   0x3 0b10 [],
                  ConstrRepr 'BTLtu  0x3 0b11 []
                ]) #-}
deriveBitPack [t|BranchType|]

data BranchNeg = BNPos | BNNeg
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data Funct3Branch = Funct3Branch BranchType BranchNeg
    deriving (Eq, Show, Generic, NFDataX, BitPack)

type Funct3 = BitVector 3

type Funct7 = BitVector 7

type RegAddr = Index 32

type CpuWord = BitVector 32
type CpuSWord = Signed 32
type CpuUWord = Unsigned 32

data Instr = Instr {
    iFunct7 :: Funct7,
    iRS2 :: RegAddr,
    iRS1 :: RegAddr,
    iFunct3 :: Funct3,
    iRD :: RegAddr,
    iOpcode :: Opcode
} deriving (Eq, Show, Generic, NFDataX, BitPack)

decodeInstr :: CpuWord -> Instr
decodeInstr = bitCoerce

