module FSM.RiscV.Wishbone where

import Clash.Prelude
import Clash.Annotations.TH

data WishboneIn = WishboneIn {
    wbDatI :: "DAT_I" ::: BitVector 32,
    wbAck  :: "ACK_I" ::: Bool
} deriving (Eq, Show, Generic, NFDataX)

data WishboneOut = WishboneOut {
    wbAdr  :: "ADR_O" ::: BitVector 32,
    wbDatO :: "DAT_O" ::: BitVector 32,
    wbWE   :: "WE_O"  ::: Bool,
    wbCyc  :: "CYC_O" ::: Bool,
    wbStb  :: "STB_O" ::: Bool
} deriving (Eq, Show, Generic, NFDataX)

emptyWishboneOut :: WishboneOut
emptyWishboneOut = WishboneOut undefined undefined undefined False False

readWishboneOut :: BitVector 32 -> WishboneOut
readWishboneOut adr = WishboneOut adr undefined False True True

writeWishboneOut :: BitVector 32 -> BitVector 32 -> WishboneOut
writeWishboneOut adr dat = WishboneOut adr dat True True True

