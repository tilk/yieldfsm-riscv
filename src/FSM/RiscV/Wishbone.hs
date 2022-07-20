{-|
Copyright  :  (C) 2022 Marek Materzok
License    :  BSD2 (see the file LICENSE)
Maintainer :  Marek Materzok <tilk@tilk.eu>

Wishbone bus definitions.
-}
module FSM.RiscV.Wishbone(
    WishboneIn(..), WishboneOut(..),
    emptyWishboneOut, readWishboneOut, writeWishboneOut,
    mkAdr, mkSel, mkDatI, mkDatO
) where

import Clash.Prelude
import FSM.RiscV.Arch

-- | Wishbone bus - input side.
data WishboneIn = WishboneIn {
    wbDatI :: "dat_i" ::: BitVector 32, -- ^ Data.
    wbAck  :: "ack_i" ::: Bool          -- ^ Acknowledge signal.
} deriving (Eq, Show, Generic, NFDataX)

-- | Wishbone bus - output side.
data WishboneOut = WishboneOut {
    wbAdr  :: "adr_o" ::: BitVector 30, -- ^ Address. Memory is word-addressed.
    wbDatO :: "dat_o" ::: BitVector 32, -- ^ Data.
    wbSel  :: "sel_o" ::: BitVector 4,  -- ^ Byte select.
    wbWE   :: "we_o"  ::: Bool,         -- ^ Write enable.
    wbCyc  :: "cyc_o" ::: Bool,         -- ^ Bus cycle signal.
    wbStb  :: "stb_o" ::: Bool          -- ^ Bus strobe signal.
} deriving (Eq, Show, Generic, NFDataX)

-- | Idle bus cycle - no operation.
emptyWishboneOut :: WishboneOut
emptyWishboneOut = WishboneOut undefined undefined undefined undefined False False

-- | Bus read operation.
readWishboneOut :: BitVector 30 -> BitVector 4 -> WishboneOut
readWishboneOut adr sel = WishboneOut adr undefined sel False True True

-- | Bus write operation.
writeWishboneOut :: BitVector 30 -> BitVector 32 -> BitVector 4 -> WishboneOut
writeWishboneOut adr dat sel = WishboneOut adr dat sel True True True

-- | Calculate word address.
mkAdr :: BitVector 32 -> BitVector 30
mkAdr addr = truncateB $ addr `shiftR` 2

-- | Calculate byte mask for given memory access width.
mwMask :: MemWidth -> BitVector 4
mwMask MWByte = 0x1
mwMask MWHalf = 0x3
mwMask MWWord = 0xf

-- | Calculate bit mask for given memory access width.
mwMaskW :: MemWidth -> BitVector 32
mwMaskW MWByte = 0xff
mwMaskW MWHalf = 0xffff
mwMaskW MWWord = 0xffffffff

-- | Get sign bits from each byte of the word. Used for sign-extending read values.
signBits :: BitVector 32 -> BitVector 4
signBits dat = v2bv $ gather (bv2v dat) $(listToVecTH [0 :: Index 32, 8, 16, 24])

-- | Given memory access width and two least significant bits of the address, calculate the amount of bits to be shifted.
mwShift :: MemWidth -> BitVector 2 -> Int
mwShift mw a = fromIntegral $ a .&. (0x3 `shiftL` fromEnum mw)

-- | Given memory access type and address, generate byte select bits for Wishbone.
mkSel :: Funct3Mem -> BitVector 32 -> BitVector 4
mkSel (Funct3Mem _ mw) addr = mwMask mw `shiftL` mwShift mw (truncateB addr)

-- | Given memory access type, address and data from the RISC-V core, generate the data value to be sent over Wishbone.
mkDatO :: Funct3Mem -> BitVector 32 -> BitVector 32 -> BitVector 32
mkDatO (Funct3Mem _ mw) addr dat = (dat .&. mwMaskW mw) `shiftL` (mwShift mw (truncateB addr) `shiftL` 3)

-- | Given memory access type, address and data received from Wishbone, generate the data to be passed to the RISC-V core.
mkDatI :: Funct3Mem -> BitVector 32 -> BitVector 32 -> BitVector 32
mkDatI (Funct3Mem ms mw) addr dat = (dat `shiftR` (shft `shiftL` 3)) .&. mwMaskW mw
                                .|. if ms == MSSigned && bitToBool (signBits dat ! (shft + (1 `shiftL` fromEnum mw) - 1)) then complement (mwMaskW mw) else 0
    where
    shft = mwShift mw (truncateB addr)

