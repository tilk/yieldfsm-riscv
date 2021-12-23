module FSM.RiscV.Wishbone where

import Clash.Prelude

data WishboneIn = WishboneIn {
    wbDatI :: Unsigned 32,
    wbAck  :: Bool
}

data WishboneOut = WishboneOut {
    wbAdr  :: Unsigned 32,
    wbDatO :: Unsigned 32,
    wbWE   :: Bool,
    wbCyc  :: Bool,
    wbStb  :: Bool
}

