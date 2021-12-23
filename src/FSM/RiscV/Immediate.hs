module FSM.RiscV.Immediate where

import Clash.Prelude
import FSM.RiscV.Arch

immediate :: CpuWord -> CpuWord
immediate a = case opcode of
        OOp     -> errorX "no immediate for OP"
        OLoad   -> immI
        OOpImm  -> immI
        OJalr   -> immI
        OStore  -> immS
        OBranch -> immB
        OAuipc  -> immU
        OLui    -> immU
        OJal    -> immJ
    where
    (k31    :: Unsigned 1,   
     k30_25 :: Unsigned 6,
     k24_21 :: Unsigned 4,
     k20    :: Unsigned 1,
     k19_12 :: Unsigned 8,
     k11_8  :: Unsigned 4,
     k7     :: Unsigned 1,
     opcode) = bitCoerce a
    immI = bitCoerce (replicate d21 k31, k30_25, k24_21, k20)
    immS = bitCoerce (replicate d21 k31, k30_25, k11_8, k7)
    immB = bitCoerce (replicate d20 k31, k7, k30_25, k11_8, low)
    immU = bitCoerce (k31, k30_25, k24_21, k20, k19_12, 0 :: Unsigned 12)
    immJ = bitCoerce (replicate d12 k31, k19_12, k20, k30_25, k24_21, low)


