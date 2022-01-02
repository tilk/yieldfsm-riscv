module FSM.RiscV.Wishbone where

import Clash.Prelude
import Clash.Annotations.TH

data WishboneIn = WishboneIn {
    wbDatI :: "dat_i" ::: BitVector 32,
    wbAck  :: "ack_i" ::: Bool
} deriving (Eq, Show, Generic, NFDataX)

data WishboneOut = WishboneOut {
    wbAdr  :: "adr_o" ::: BitVector 32,
    wbDatO :: "dat_o" ::: BitVector 32,
    wbWE   :: "we_o"  ::: Bool,
    wbCyc  :: "cyc_o" ::: Bool,
    wbStb  :: "stb_o" ::: Bool
} deriving (Eq, Show, Generic, NFDataX)

emptyWishboneOut :: WishboneOut
emptyWishboneOut = WishboneOut undefined undefined undefined False False

readWishboneOut :: BitVector 32 -> WishboneOut
readWishboneOut adr = WishboneOut adr undefined False True True

writeWishboneOut :: BitVector 32 -> BitVector 32 -> WishboneOut
writeWishboneOut adr dat = WishboneOut adr dat True True True

