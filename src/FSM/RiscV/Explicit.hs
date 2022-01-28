module FSM.RiscV.Explicit(
    PCSel(..), AddrSel(..), RDSel(..), AluASel(..), AluBSel(..),
    ExplicitControl(..), ExplicitStatus(..), AluControl(..),
    defaultControl, explicitDatapath, explicitControl
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

data ExplicitState = ESFetch | ESDecode | ESExec | ESExecImm | ESAluWB
                   | ESMemAddr | ESMemRead | ESMemWrite | ESMemWB
                   | ESBranch | ESJal | ESJalr | ESLui
    deriving (Eq, Show, Generic, NFDataX, BitPack)

explicitControl :: HiddenClockResetEnable dom
                => Signal dom ExplicitStatus
                -> Signal dom ExplicitControl
explicitControl = mealy (\s i -> (transition s i, output s i)) ESFetch

transition :: ExplicitState -> ExplicitStatus -> ExplicitState
transition ESFetch st | esWbAck st = ESDecode
                      | otherwise  = ESFetch
transition ESDecode st = case esOpcode st of
    OLoad   -> ESMemAddr
    OStore  -> ESMemAddr
    OBranch -> ESBranch
    OJal    -> ESJal
    OJalr   -> ESJalr
    OOp     -> ESExec
    OOpImm  -> ESExecImm
    OLui    -> ESLui
    OAuipc  -> ESAluWB
transition ESMemAddr st = case esOpcode st of
    OLoad  -> ESMemRead
    OStore -> ESMemWrite
    _      -> undefined
transition ESMemRead st  | esWbAck st = ESMemWB
                         | otherwise  = ESMemRead
transition ESMemWrite st | esWbAck st = ESFetch
                         | otherwise  = ESMemWrite
transition ESExec    _  = ESAluWB
transition ESExecImm _  = ESAluWB
transition ESLui     _  = ESFetch
transition ESJal     _  = ESFetch
transition ESJalr    _  = ESFetch
transition ESAluWB   _  = ESFetch
transition ESMemWB   _  = ESFetch
transition ESBranch  _  = ESFetch

output :: ExplicitState -> ExplicitStatus -> ExplicitControl
output s st = o s
    where
    alui = decodeAluInstr (esOpcode st) (esFunct3 st) (esFunct7 st)
    o ESFetch = defaultControl {
        ecAddrSel = Just AddrSelPC,
        ecIRWE = esWbAck st,
        ecAluCtl = Just $ AluControl aiAdd AluASelPC AluBSel4 }
    o ESDecode = defaultControl { 
        ecPCSel = Just PCSelAR,
        ecAluCtl = Just $ AluControl aiAdd AluASelPC AluBSelImm,
        ecRSWE = True }
    o ESExec = defaultControl {
        ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelRS2 }
    o ESExecImm = defaultControl {
        ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelImm }
    o ESLui = defaultControl {
        ecRDSel = Just RDSelImm }
    o ESAluWB = defaultControl {
        ecRDSel = Just RDSelAR }
    o ESMemAddr = defaultControl {
        ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }
    o ESMemRead = defaultControl {
        ecAddrSel = Just AddrSelAR,
        ecDRWE = esWbAck st }
    o ESMemWrite = defaultControl {
        ecAddrSel = Just AddrSelAR,
        ecWbWE = True }
    o ESMemWB = defaultControl {
        ecRDSel = Just RDSelDR }
    o ESBranch = defaultControl {
        ecPCSel = if esAluLSB st then Just PCSelAR else Nothing,
        ecAluCtl = Just $ AluControl alui AluASelRS1 AluBSelRS2 }
    o ESJal = defaultControl {
        ecPCSel = Just PCSelAR,
        ecRDSel = Just RDSelPC }
    o ESJalr = defaultControl {
        ecPCSel = Just PCSelAlu,
        ecRDSel = Just RDSelPC,
        ecAluCtl = Just $ AluControl aiAdd AluASelRS1 AluBSelImm }


