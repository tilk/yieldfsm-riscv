module FSM.RiscV.Wishbone where

import Clash.Prelude

data WishboneIn = WishboneIn {
    wbDatI :: BitVector 32,
    wbAck  :: Bool
} deriving (Eq, Show, Generic, NFDataX)

data WishboneOut = WishboneOut {
    wbAdr  :: BitVector 32,
    wbDatO :: BitVector 32,
    wbWE   :: Bool,
    wbCyc  :: Bool,
    wbStb  :: Bool
} deriving (Eq, Show, Generic, NFDataX)

emptyWishboneOut :: WishboneOut
emptyWishboneOut = WishboneOut undefined undefined undefined False False

readWishboneOut :: BitVector 32 -> WishboneOut
readWishboneOut adr = WishboneOut adr undefined False True True

writeWishboneOut :: BitVector 32 -> BitVector 32 -> WishboneOut
writeWishboneOut adr dat = WishboneOut adr dat True True True

