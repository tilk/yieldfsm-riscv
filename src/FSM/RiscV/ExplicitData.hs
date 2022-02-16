module FSM.RiscV.ExplicitData(
    PCSel(..), AddrSel(..), RDSel(..), AluASel(..), AluBSel(..),
    ExplicitControl(..), ExplicitStatus(..), AluControl(..),
    defaultControl, explicitDatapath
) where

import Clash.Prelude
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.RegFile
import FSM.RiscV.Wishbone
import FSM.RiscV.Immediate
import Data.Maybe(isJust, fromJust)

data PCSel = PCSelAlu | PCSelAR
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data AddrSel = AddrSelAR | AddrSelPC
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data RDSel = RDSelAR | RDSelDR | RDSelPC | RDSelImm
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data AluASel = AluASelRS1 | AluASelPC
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data AluBSel = AluBSelRS2 | AluBSelImm | AluBSel4
    deriving (Eq, Show, Generic, NFDataX, BitPack)

data AluControl = AluControl {
    acAluI    :: AluInstr,
    acAluASel :: AluASel,
    acAluBSel :: AluBSel
} deriving (Eq, Show, Generic, NFDataX, BitPack)

data ExplicitControl = ExplicitControl {
    ecPCSel   :: Maybe PCSel,
    ecAddrSel :: Maybe AddrSel,
    ecRDSel   :: Maybe RDSel,
    ecAluCtl  :: Maybe AluControl,
    ecIRWE    :: Bool,
    ecDRWE    :: Bool,
    ecRSWE    :: Bool,
    ecWbWE    :: Bool
} deriving (Eq, Show, Generic, NFDataX, BitPack)

data ExplicitStatus = ExplicitStatus {
    esOpcode :: Opcode,
    esFunct3 :: Funct3,
    esFunct7 :: Funct7,
    esAluLSB :: Bool,
    esWbAck  :: Bool
} deriving (Eq, Show, Generic, NFDataX, BitPack)

defaultControl :: ExplicitControl
defaultControl = 
    ExplicitControl Nothing Nothing Nothing Nothing
                    False False False False

mux2 :: (Applicative f, BitPack a, BitSize a ~ 1)
     => f a -> f b -> f b -> f b
mux2 = mux . fmap bitCoerce

mux2m :: (Applicative f, BitPack a, BitSize a ~ 1)
      => f (Maybe a) -> f b -> f b -> f (Maybe b)
mux2m = liftA3 f
    where
    f Nothing _ _  = Nothing
    f (Just k) a b = if bitCoerce k then Just a else Just b

mux4 :: (Applicative f, BitPack a, BitSize a ~ 2)
     => f a -> f b -> f b -> f b -> f b -> f b
mux4 s a b c d = mux (fst <$> s') (mux (snd <$> s') a b) (mux (snd <$> s') c d)
    where
    s' = bitCoerce <$> s

explicitDatapath :: HiddenClockResetEnable dom
                 => CpuWord
                 -> Signal dom ExplicitControl
                 -> Signal dom WishboneIn
                 -> (Signal dom ExplicitStatus, Signal dom WishboneOut)
explicitDatapath startPC ctl wb =
    (ExplicitStatus <$> (iOpcode <$> instr)
                    <*> (iFunct3 <$> instr)
                    <*> (iFunct7 <$> instr)
                    <*> ((bitCoerce . lsb) <$> aluOut)
                    <*> (wbAck <$> wb),
     WishboneOut <$> (mkAdr <$> addr)
                 <*> (mkDatO <$> f3m <*> addr <*> (rfRS2V <$> rf))
                 <*> (mux2 (ecIRWE <$> ctl) (pure $ -1) (mkSel <$> f3m <*> addr))
                 <*> (ecWbWE <$> ctl)
                 <*> (isJust <$> maddr)
                 <*> (isJust <$> maddr))
    where
    pc = regMaybe startPC $ mux2m (ecPCSel <$> ctl) ar aluOut
    ir = regEn undefined (ecIRWE <$> ctl) (wbDatI <$> wb)
    ar = regEn undefined (isJust . ecAluCtl <$> ctl) aluOut
    dr = register undefined (mkDatI <$> f3m <*> addr <*> (wbDatI <$> wb))
    instr = decodeInstr <$> ir
    f3m = bitCoerce . iFunct3 <$> instr
    imm = immediate <$> ir
    maddr = mux2m (ecAddrSel <$> ctl) pc ar
    addr = fromJust <$> maddr
    rf = regFile $ RegFileIn <$> (ecRSWE <$> ctl) <*> (isJust . ecRDSel <$> ctl)
                             <*> (iRS1 <$> instr) <*> (iRS2 <$> instr)
                             <*> (iRD <$> instr)
                             <*> mux4 (fromJust . ecRDSel <$> ctl) imm pc dr ar
    aluctl = fromJust . ecAluCtl <$> ctl
    aluOut = alu <$> (acAluI <$> aluctl)
                 <*> mux2 (acAluASel <$> aluctl) pc (rfRS1V <$> rf)
                 <*> mux4 (acAluBSel <$> aluctl) (pure undefined) (pure 4) imm (rfRS2V <$> rf)

