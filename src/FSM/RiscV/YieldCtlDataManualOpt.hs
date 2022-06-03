module FSM.RiscV.YieldCtlDataManualOpt(
    rvfsmManualOpt
) where

import Clash.Prelude
import Clash.Annotations.BitRepresentation
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.RegFile
import FSM.RiscV.Immediate
import FSM.RiscV.Wishbone

data State
  = SuseAlu_pc4
  | SuseAlu_op
  | Swhile_load
  | Swhile_instr
  | Swhile0
  | Sforever
  | Sforever0
  | Sforever1
  | Sforever2
  | Sforever3
  deriving (Show, Generic, NFDataX)

data Registers = Registers {
    rState :: State,
    rPC :: CpuWord {-pc-},
    rInstr :: Instr,
    rAdr :: (BitVector 30) {-adr-},
    rSel :: (BitVector 4) {-sel-},
    rAddr :: CpuWord {-addr-},
    rFunct3Mem :: Funct3Mem,
    rAluInstr :: AluInstr,
    rImm :: CpuWord {-imm-},
    rA :: CpuWord,
    rB :: CpuWord,
    rAli :: CpuWord,
    rDat :: CpuWord,
    rWbdat :: CpuWord
} deriving (Show, Generic, NFDataX)

data FSMState
  = CuseAlu_rvfsm (CTrvfsmuseAlu, CpuWord {-a-}, CpuWord {-b-})
  | Cwhile_rvfsm (CpuWord {-pc?-}, BitVector 30 {-adr-}, BitVector 4 {-sel-}, CTrvfsmwbRead)
  | Cwhile_rvfsm0 (CpuWord {-pc-}, BitVector 30 {-adr-}, BitVector 4 {-sel-}, CpuWord {-dat-})
  | Cforever_rvfsm (CpuWord {-pc-}, Instr, CpuWord {-r-})
  | Cforever_rvfsm0 (CpuWord {-pc-}, Instr, CpuWord {-r-})
  | Cforever_rvfsm1 (CpuWord {-pc-}, Instr, CpuWord {-r-})
  | Cforever_rvfsm2 (CpuWord {-pc-}, Instr, CpuWord {-r-})
  | Cforever_rvfsm3 (CpuWord {-pc-}, Instr, CpuWord {-r-}, AluInstr, CpuWord {-imm-})
  deriving (Show, Generic, NFDataX)

data CTrvfsmwbRead
  = CrvfsmwbRead_load (Instr, Funct3Mem, CpuWord {-addr-})
  | CrvfsmwbRead_instr (CpuWord {-r-})
  deriving (Show, Generic, NFDataX)

data CTrvfsmuseAlu
  = CrvfsmuseAlu_pc4 ()
  | CrvfsmuseAlu_op (CpuWord {-pc-}, Instr, CpuWord {-imm-}, AluInstr)
  deriving (Show, Generic, NFDataX)

state2regs :: FSMState -> Registers
state2regs (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), a, b)) = Registers { rState = SuseAlu_pc4, rA = a, rB = b }
state2regs (CuseAlu_rvfsm (CrvfsmuseAlu_op (pc, instr, imm, alui), a, b)) = Registers { rState = SuseAlu_op, rInstr = instr, rImm = imm, rAluInstr = alui, rA = a, rB = b, rPC = pc }
state2regs (Cwhile_rvfsm (pc, adr, sel, CrvfsmwbRead_load (instr, f3mem, addr))) = Registers { rState = Swhile_load, rPC = pc, rAdr = adr, rSel = sel, rInstr = instr, rFunct3Mem = f3mem, rAddr = addr }
state2regs (Cwhile_rvfsm (pc, adr, sel, CrvfsmwbRead_instr ali)) = Registers { rState = Swhile_instr, rPC = pc, rAdr = adr, rSel = sel, rAli = ali }
state2regs (Cwhile_rvfsm0 (pc, adr, sel, dat)) = Registers { rState = Swhile0, rPC = pc, rAdr = adr, rDat = dat, rSel = sel }
state2regs (Cforever_rvfsm (pc, instr, r)) = Registers { rState = Sforever, rPC = pc, rInstr = instr, rWbdat = r }
state2regs (Cforever_rvfsm0 (pc, instr, ali)) = Registers { rState = Sforever0, rPC = pc, rInstr = instr, rAli = ali }
state2regs (Cforever_rvfsm1 (pc, instr, ali)) = Registers { rState = Sforever1, rPC = pc, rInstr = instr, rAli = ali }
state2regs (Cforever_rvfsm2 (pc, instr, ali)) = Registers { rState = Sforever2, rPC = pc, rInstr = instr, rAli = ali }
state2regs (Cforever_rvfsm3 (pc, instr, imm, alui, ali)) = Registers { rState = Sforever3, rPC = pc, rInstr = instr, rAli = ali, rAluInstr = alui, rImm = imm }

regs2state :: Registers -> FSMState
regs2state (Registers { rState = SuseAlu_pc4, rA = a, rB = b }) = CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), a, b)
regs2state (Registers { rState = SuseAlu_op, rInstr = instr, rImm = imm, rAluInstr = alui, rA = a, rB = b, rPC = pc }) = CuseAlu_rvfsm (CrvfsmuseAlu_op (pc, instr, imm, alui), a, b)
regs2state (Registers { rState = Swhile_load, rPC = pc, rAdr = adr, rSel = sel, rInstr = instr, rFunct3Mem = f3mem, rAddr = addr }) = Cwhile_rvfsm (pc, adr, sel, CrvfsmwbRead_load (instr, f3mem, addr))
regs2state (Registers { rState = Swhile_instr, rPC = pc, rAdr = adr, rSel = sel, rAli = ali }) = Cwhile_rvfsm (pc, adr, sel, CrvfsmwbRead_instr ali)
regs2state (Registers { rState = Swhile0, rPC = pc, rAdr = adr, rDat = dat, rSel = sel }) = Cwhile_rvfsm0 (pc, adr, sel, dat)
regs2state (Registers { rState = Sforever, rPC = pc, rInstr = instr, rWbdat = r }) = Cforever_rvfsm (pc, instr, r)
regs2state (Registers { rState = Sforever0, rPC = pc, rInstr = instr, rAli = ali }) = Cforever_rvfsm0 (pc, instr, ali)
regs2state (Registers { rState = Sforever1, rPC = pc, rInstr = instr, rAli = ali }) = Cforever_rvfsm1 (pc, instr, ali)
regs2state (Registers { rState = Sforever2, rPC = pc, rInstr = instr, rAli = ali }) = Cforever_rvfsm2 (pc, instr, ali)
regs2state (Registers { rState = Sforever3, rPC = pc, rInstr = instr, rAli = ali, rAluInstr = alui, rImm = imm }) = Cforever_rvfsm3 (pc, instr, imm, alui, ali)

rvfsmManualOpt ::
  HiddenClockResetEnable dom =>
  CpuWord
  -> Signal dom (WishboneIn, RegFileOut, AluOut)
     -> Signal dom (WishboneOut, RegFileIn, AluIn)
rvfsmManualOpt startPC
  = (mealy fsmFunc_rvfsm_a1QRs) fsmInitState_rvfsm_a1QRq
  where
      fsmInitState_rvfsm_a1QRq
        = state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), startPC, 4)
      fsmFunc_rvfsm_a1QRs
        (regs2state -> CuseAlu_rvfsm (c_a1QDK, a_a1Qwv, b_a1Qww))
        (wb, _, ali)
        = let v_a1QQG = ((doAluIn aiAdd) a_a1Qwv) b_a1Qww
          in
            case c_a1QDK of
              CrvfsmuseAlu_op (r_a1QzR, instr_a1QA5, imm_a1QA4,
                                    aluInstr_a1QA6)
                -> (state2regs $ Cforever_rvfsm3
                      (r_a1QzR, instr_a1QA5, ali, aluInstr_a1QA6, imm_a1QA4), 
                    (emptyWishboneOut, 
                     (readRegFileIn (iRS1 instr_a1QA5)) (iRS2 instr_a1QA5), v_a1QQG))
              CrvfsmuseAlu_pc4 ()
                -> let sel_a1QQf = negate 1 in
                   let adr_a1QQe = mkAdr a_a1Qwv
                   in
                     if wbAck wb then
                         let r_a1QzY = wbDatI wb in
                         let imm_a1QA4 = immediate r_a1QzY in
                         let instr_a1QA5 = decodeInstr r_a1QzY
                         in
                           (state2regs $ CuseAlu_rvfsm
                              (
                               CrvfsmuseAlu_op
                                 (ali, instr_a1QA5, imm_a1QA4,
                                  ((decodeAluInstr (iOpcode instr_a1QA5)) (iFunct3 instr_a1QA5))
                                    (iFunct7 instr_a1QA5)), a_a1Qwv, imm_a1QA4), 
                            ((readWishboneOut adr_a1QQe) sel_a1QQf, emptyRegFileIn, v_a1QQG))
                     else
                         (state2regs $ Cwhile_rvfsm
                            (a_a1Qwv, adr_a1QQe, sel_a1QQf, CrvfsmwbRead_instr ali), 
                          ((readWishboneOut adr_a1QQe) sel_a1QQf, emptyRegFileIn, v_a1QQG))
      fsmFunc_rvfsm_a1QRs
        (regs2state -> Cwhile_rvfsm (v_a1QwQ, adr_a1QwD, sel_a1QwE, c_a1QDT))
        (wb, _, _)
        = if wbAck wb then
              let v_a1QQK = wbDatI wb
              in
                case c_a1QDT of
                  CrvfsmwbRead_instr r_a1QzR
                    -> let imm_a1QA4 = immediate v_a1QQK in
                       let instr_a1QA5 = decodeInstr v_a1QQK
                       in
                         (state2regs $ CuseAlu_rvfsm
                            (
                             CrvfsmuseAlu_op
                               (r_a1QzR, instr_a1QA5, imm_a1QA4,
                                ((decodeAluInstr (iOpcode instr_a1QA5)) (iFunct3 instr_a1QA5))
                                  (iFunct7 instr_a1QA5)), v_a1QwQ, imm_a1QA4), 
                          ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                           emptyAluIn))
                  CrvfsmwbRead_load (rc_a1QEB, f3_a1Qx0, addr_a1Qx1)
                    -> (state2regs $ Cforever_rvfsm
                          (v_a1QwQ, rc_a1QEB, ((mkDatI f3_a1Qx0) addr_a1Qx1) v_a1QQK), 
                        ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                         emptyAluIn))
          else
              (state2regs $ Cwhile_rvfsm (v_a1QwQ, adr_a1QwD, sel_a1QwE, c_a1QDT), 
               ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (regs2state -> Cwhile_rvfsm0 (v_a1Qxr, adr_a1Qxd, sel_a1Qxf, dat_a1Qxe))
        (wb, _, _)
        = if wbAck wb then
              (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), v_a1Qxr, 4), 
               (((writeWishboneOut adr_a1Qxd) dat_a1Qxe) sel_a1Qxf, 
                emptyRegFileIn, emptyAluIn))
          else
              (state2regs $ Cwhile_rvfsm0 (v_a1Qxr, adr_a1Qxd, sel_a1Qxf, dat_a1Qxe), 
               (((writeWishboneOut adr_a1Qxd) dat_a1Qxe) sel_a1Qxf, 
                emptyRegFileIn, emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (regs2state -> Cforever_rvfsm (v_a1QAn, instr_a1QA5, r_a1QAm))
        (_, _, _)
        = (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), v_a1QAn, 4), 
           (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QAm, 
            emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (regs2state -> Cforever_rvfsm0 (pc_a1QzL, instr_a1QA5, r_a1QAO))
        (wb, rf, _)
        = let f3_a1QFE = bitCoerce $ iFunct3 instr_a1QA5 in
          let sel_a1QFP = (mkSel f3_a1QFE) r_a1QAO in
          let dat_a1QFO = ((mkDatO f3_a1QFE) r_a1QAO) (rfRS2V rf) in
          let adr_a1QFN = mkAdr r_a1QAO
          in
            if wbAck wb then
                (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
                 (((writeWishboneOut adr_a1QFN) dat_a1QFO) sel_a1QFP, 
                  emptyRegFileIn, emptyAluIn))
            else
                (state2regs $ Cwhile_rvfsm0 (pc_a1QzL, adr_a1QFN, sel_a1QFP, dat_a1QFO), 
                 (((writeWishboneOut adr_a1QFN) dat_a1QFO) sel_a1QFP, 
                  emptyRegFileIn, emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (regs2state -> Cforever_rvfsm1 (pc_a1QzL, instr_a1QA5, r_a1QBj))
        (_, _, _)
        = (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
           (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QBj, 
            emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (regs2state -> Cforever_rvfsm2 (pc_a1QzL, instr_a1QA5, r_a1QBG))
        (_, _, _)
        = (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
           (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QBG, 
            emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (regs2state -> Cforever_rvfsm3 (pc_a1QzL, instr_a1QA5, r_a1QA8, aluInstr_a1QA6,
                          imm_a1QA4))
        (_, rf, ali)
        = case iOpcode instr_a1QA5 of
            OLoad
              -> let f3_a1QGL = bitCoerce $ iFunct3 instr_a1QA5
                 in
                   (state2regs $ Cwhile_rvfsm
                      (pc_a1QzL, mkAdr ali, (mkSel f3_a1QGL) ali,
                       CrvfsmwbRead_load (instr_a1QA5, f3_a1QGL, ali)), 
                    (emptyWishboneOut, emptyRegFileIn, 
                     ((doAluIn aiAdd) (rfRS1V rf)) imm_a1QA4))
            OStore
              -> (state2regs $ Cforever_rvfsm0 (pc_a1QzL, instr_a1QA5, ali), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aiAdd) (rfRS1V rf)) imm_a1QA4))
            OOp
              -> (state2regs $ Cforever_rvfsm1 (pc_a1QzL, instr_a1QA5, ali), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aluInstr_a1QA6) (rfRS1V rf)) (rfRS2V rf)))
            OOpImm
              -> (state2regs $ Cforever_rvfsm2 (pc_a1QzL, instr_a1QA5, ali), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aluInstr_a1QA6) (rfRS1V rf)) imm_a1QA4))
            OBranch
              -> (state2regs $ CuseAlu_rvfsm
                    (CrvfsmuseAlu_pc4 (), if ali == 0 then pc_a1QzL else r_a1QA8, 4), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aluInstr_a1QA6) (rfRS1V rf)) (rfRS2V rf)))
            OLui
              -> (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) imm_a1QA4, 
                   emptyAluIn))
            OJal
              -> (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), r_a1QA8, 4), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) pc_a1QzL, 
                   emptyAluIn))
            OJalr
              -> (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), ali, 4), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) pc_a1QzL, 
                   ((doAluIn aiAdd) (rfRS1V rf)) imm_a1QA4))
            OAuipc
              -> (state2regs $ CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QA8, 
                   emptyAluIn))


