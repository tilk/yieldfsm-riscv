module FSM.RiscV.Wishbone(
    WishboneIn(..), WishboneOut(..),
    emptyWishboneOut, readWishboneOut, writeWishboneOut,
    mkAdr, mkSel, mkDatI, mkDatO
) where

import Clash.Prelude
import FSM.RiscV.Arch

data WishboneIn = WishboneIn {
    wbDatI :: "dat_i" ::: BitVector 32,
    wbAck  :: "ack_i" ::: Bool
} deriving (Eq, Show, Generic, NFDataX)

data WishboneOut = WishboneOut {
    wbAdr  :: "adr_o" ::: BitVector 30,
    wbDatO :: "dat_o" ::: BitVector 32,
    wbSel  :: "sel_o" ::: BitVector 4,
    wbWE   :: "we_o"  ::: Bool,
    wbCyc  :: "cyc_o" ::: Bool,
    wbStb  :: "stb_o" ::: Bool
} deriving (Eq, Show, Generic, NFDataX)

emptyWishboneOut :: WishboneOut
emptyWishboneOut = WishboneOut undefined undefined undefined undefined False False

readWishboneOut :: BitVector 30 -> BitVector 4 -> WishboneOut
readWishboneOut adr sel = WishboneOut adr undefined sel False True True

writeWishboneOut :: BitVector 30 -> BitVector 32 -> BitVector 4 -> WishboneOut
writeWishboneOut adr dat sel = WishboneOut adr dat sel True True True

mkAdr :: BitVector 32 -> BitVector 30
mkAdr addr = truncateB $ addr `shiftR` 2

mwMask :: MemWidth -> BitVector 4
mwMask MWByte = 0x1
mwMask MWHalf = 0x3
mwMask MWWord = 0xf

mwMaskW :: MemWidth -> BitVector 32
mwMaskW MWByte = 0xff
mwMaskW MWHalf = 0xffff
mwMaskW MWWord = 0xffffffff

signBits :: BitVector 32 -> BitVector 4
signBits dat = v2bv $ gather (bv2v dat) $(listToVecTH [0 :: Index 32, 8, 16, 24])

mwShift :: MemWidth -> BitVector 2 -> Int
mwShift mw a = fromIntegral $ a .&. (0x3 `shiftL` fromEnum mw)

mkSel :: Funct3Mem -> BitVector 32 -> BitVector 4
mkSel (Funct3Mem _ mw) addr = mwMask mw `shiftL` mwShift mw (truncateB addr)

mkDatO :: Funct3Mem -> BitVector 32 -> BitVector 32 -> BitVector 32
mkDatO (Funct3Mem _ mw) addr dat = (dat .&. mwMaskW mw) `shiftL` (mwShift mw (truncateB addr) `shiftL` 3)

mkDatI :: Funct3Mem -> BitVector 32 -> BitVector 32 -> BitVector 32
mkDatI (Funct3Mem ms mw) addr dat = (dat `shiftR` (shft `shiftL` 3)) .&. mwMaskW mw
                                .|. if ms == MSSigned && bitToBool (signBits dat ! (shft + (1 `shiftL` fromEnum mw) - 1)) then complement (mwMaskW mw) else 0
    where
    shft = mwShift mw (truncateB addr)

