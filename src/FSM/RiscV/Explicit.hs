module FSM.RiscV.Explicit(
    PCSel(..), AddrSel(..), RDSel(..), AluASel(..), AluBSel(..),
    ExplicitControl(..), ExplicitStatus(..), defaultControl,
    explicitDatapath, explicitControl
) where

import Clash.Prelude
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.RegFile
import FSM.RiscV.Wishbone
import FSM.RiscV.Immediate

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

data ExplicitControl = ExplicitControl {
    ecPCSel   :: PCSel,
    ecAddrSel :: AddrSel,
    ecRDSel   :: RDSel,
    ecAluASel :: AluASel,
    ecAluBSel :: AluBSel,
    ecAluI    :: AluInstr,
    ecPCWE    :: Bool,
    ecIRWE    :: Bool,
    ecDRWE    :: Bool,
    ecRSWE    :: Bool,
    ecRDWE    :: Bool,
    ecARWE    :: Bool,
    ecWbWE    :: Bool,
    ecWbCyc   :: Bool,
    ecWbStb   :: Bool
} deriving (Eq, Show, Generic, NFDataX, BitPack)

data ExplicitStatus = ExplicitStatus {
    esOpcode  :: Opcode,
    esFunct3  :: Funct3,
    esFunct7  :: Funct7,
    esAluZero :: Bool,
    esWbAck   :: Bool
} deriving (Eq, Show, Generic, NFDataX, BitPack)

defaultControl :: ExplicitControl
defaultControl = 
    ExplicitControl undefined undefined undefined undefined undefined undefined
                    False False False False False False False False False

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
                    <*> ((== 0) <$> aluOut)
                    <*> (wbAck <$> wb),
     WishboneOut <$> (mkAdr <$> addr)
                 <*> (mkDatO <$> f3m <*> addr <*> (rfRS2V <$> rf))
                 <*> (mkSel <$> f3m <*> addr)
                 <*> (ecWbWE <$> ctl)
                 <*> (ecWbCyc <$> ctl)
                 <*> (ecWbStb <$> ctl))
    where
    pc = regEn startPC (ecPCWE <$> ctl) $ mux2 (ecPCSel <$> ctl) ar aluOut
    ir = regEn undefined (ecIRWE <$> ctl) (wbDatI <$> wb)
    ar = regEn undefined (ecARWE <$> ctl) aluOut
    dr = register undefined (mkDatI <$> f3m <*> addr <*> (wbDatI <$> wb))
    instr = decodeInstr <$> ir
    f3m = bitCoerce . iFunct3 <$> instr
    imm = immediate <$> ir
    addr = mux2 (ecAddrSel <$> ctl) pc ar
    rf = regFile $ RegFileIn <$> (ecRSWE <$> ctl) <*> (ecRDWE <$> ctl)
                             <*> (iRS1 <$> instr) <*> (iRS2 <$> instr)
                             <*> (iRD <$> instr)
                             <*> mux4 (ecRDSel <$> ctl) imm pc dr ar
    aluOut = alu <$> (ecAluI <$> ctl)
                 <*> mux2 (ecAluASel <$> ctl) pc (rfRS1V <$> rf)
                 <*> mux4 (ecAluBSel <$> ctl) (pure undefined) (pure 4) imm (rfRS2V <$> rf)

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
        ecAddrSel = AddrSelPC,
        ecWbCyc = True,
        ecWbStb = True,
        ecIRWE = esWbAck st,
        ecAluI = aiAdd,
        ecAluASel = AluASelPC,
        ecAluBSel = AluBSel4,
        ecARWE = True
    }
    o ESDecode = defaultControl { 
        ecPCSel = PCSelAR,
        ecPCWE = True,
        ecAluI = aiAdd,
        ecAluASel = AluASelPC,
        ecAluBSel = AluBSelImm,
        ecARWE = True
    }
    o ESExec = defaultControl {
        ecAluI = alui,
        ecAluASel = AluASelRS1,
        ecAluBSel = AluBSelRS2,
        ecARWE = True
    }
    o ESExecImm = defaultControl {
        ecAluI = alui,
        ecAluASel = AluASelRS1,
        ecAluBSel = AluBSelImm,
        ecARWE = True
    }
    o ESLui = defaultControl {
        ecRDSel = RDSelImm,
        ecRDWE = True
    }
    o ESAluWB = defaultControl {
        ecRDSel = RDSelAR,
        ecRDWE = True
    }
    o ESMemAddr = defaultControl {
        ecAluI = aiAdd,
        ecAluASel = AluASelRS1,
        ecAluBSel = AluBSelImm,
        ecARWE = True
    }
    o ESMemRead = defaultControl {
        ecAddrSel = AddrSelAR,
        ecWbCyc = True,
        ecWbStb = True,
        ecDRWE = True
    }
    o ESMemWrite = defaultControl {
        ecAddrSel = AddrSelAR,
        ecWbCyc = True,
        ecWbStb = True,
        ecWbWE = True
    }
    o ESMemWB = defaultControl {
        ecRDSel = RDSelDR,
        ecRDWE = True
    }
    o ESBranch = defaultControl {
        ecPCSel = PCSelAR,
        ecPCWE = esAluZero st,
        ecAluI = alui,
        ecAluASel = AluASelRS1,
        ecAluBSel = AluBSelRS2,
        ecARWE = True
    }
    o ESJal = defaultControl {
        ecPCSel = PCSelAR,
        ecPCWE = True,
        ecRDSel = RDSelPC,
        ecRDWE = True
    }
    o ESJalr = defaultControl {
        ecPCSel = PCSelAlu,
        ecPCWE = True,
        ecRDSel = RDSelPC,
        ecRDWE = True,
        ecAluI = aiAdd,
        ecAluASel = AluASelRS1,
        ecAluBSel = AluBSelImm
    }
    
    
