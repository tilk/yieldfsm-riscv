module FSM.RiscV.Wishbone where

import Clash.Prelude

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

wbWordAddr :: BitVector 32 -> BitVector 30
wbWordAddr addr = truncateB $ addr `shiftR` 2

