module FSM.RiscV.YieldCtlDataManualOpt(
    rvfsmManualOpt
) where

import Clash.Prelude
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.RegFile
import FSM.RiscV.Immediate
import FSM.RiscV.Wishbone

data FSMState_rvfsm
  = CuseAlu_rvfsm (CTrvfsmuseAlu, CpuWord {-a-}, CpuWord {-b-})
  | Cwhile_rvfsm (CpuWord {-pc?-}, BitVector 30 {-adr-}, BitVector 4 {-sel-}, CTrvfsmwbRead)
  | Cwhile_rvfsm0 (CpuWord {-pc-}, BitVector 30 {-adr-}, CpuWord {-dat-}, BitVector 4 {-sel-})
  | Cforever_rvfsm (CpuWord {-pc-}, Instr, CpuWord {-r-})
  | Cforever_rvfsm0 (CpuWord {-pc-}, Instr, CpuWord {-r-})
  | Cforever_rvfsm1 (CpuWord {-pc-}, Instr, CpuWord {-r-})
  | Cforever_rvfsm2 (CpuWord {-pc-}, Instr, CpuWord {-r-})
  | Cforever_rvfsm3 (CpuWord {-pc-}, Instr, CpuWord {-imm-}, AluInstr, CpuWord {-r-})
  deriving (Show, Generic, NFDataX)

data CTrvfsmwbRead
  = CrvfsmwbRead_load (Instr, Funct3Mem, CpuWord {-addr-})
  | CrvfsmwbRead_instr (CpuWord {-r-})
  deriving (Show, Generic, NFDataX)

data CTrvfsmuseAlu
  = CrvfsmuseAlu_pc4 ()
  | CrvfsmuseAlu_op (CpuWord {-pc-}, Instr, CpuWord {-imm-}, AluInstr)
  deriving (Show, Generic, NFDataX)

rvfsmManualOpt ::
  HiddenClockResetEnable dom =>
  CpuWord
  -> Signal dom (WishboneIn, RegFileOut, AluOut)
     -> Signal dom (WishboneOut, RegFileIn, AluIn)
rvfsmManualOpt startPC
  = (mealy fsmFunc_rvfsm_a1QRs) fsmInitState_rvfsm_a1QRq
  where
      fsmInitState_rvfsm_a1QRq
        = CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), startPC, 4)
      fsmFunc_rvfsm_a1QRs
        (CuseAlu_rvfsm (c_a1QDK, a_a1Qwv, b_a1Qww))
        (wb, _, ali)
        = let v_a1QQG = ((doAluIn aiAdd) a_a1Qwv) b_a1Qww
          in
            case c_a1QDK of
              CrvfsmuseAlu_op (r_a1QzR, instr_a1QA5, imm_a1QA4,
                                    aluInstr_a1QA6)
                -> (Cforever_rvfsm3
                      (r_a1QzR, instr_a1QA5, imm_a1QA4, aluInstr_a1QA6, ali), 
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
                           (CuseAlu_rvfsm
                              (
                               CrvfsmuseAlu_op
                                 (ali, instr_a1QA5, imm_a1QA4,
                                  ((decodeAluInstr (iOpcode instr_a1QA5)) (iFunct3 instr_a1QA5))
                                    (iFunct7 instr_a1QA5)), a_a1Qwv, imm_a1QA4), 
                            ((readWishboneOut adr_a1QQe) sel_a1QQf, emptyRegFileIn, v_a1QQG))
                     else
                         (Cwhile_rvfsm
                            (a_a1Qwv, adr_a1QQe, sel_a1QQf, CrvfsmwbRead_instr ali), 
                          ((readWishboneOut adr_a1QQe) sel_a1QQf, emptyRegFileIn, v_a1QQG))
      fsmFunc_rvfsm_a1QRs
        (Cwhile_rvfsm (v_a1QwQ, adr_a1QwD, sel_a1QwE, c_a1QDT))
        (wb, _, _)
        = if wbAck wb then
              let v_a1QQK = wbDatI wb
              in
                case c_a1QDT of
                  CrvfsmwbRead_instr r_a1QzR
                    -> let imm_a1QA4 = immediate v_a1QQK in
                       let instr_a1QA5 = decodeInstr v_a1QQK
                       in
                         (CuseAlu_rvfsm
                            (
                             CrvfsmuseAlu_op
                               (r_a1QzR, instr_a1QA5, imm_a1QA4,
                                ((decodeAluInstr (iOpcode instr_a1QA5)) (iFunct3 instr_a1QA5))
                                  (iFunct7 instr_a1QA5)), v_a1QwQ, imm_a1QA4), 
                          ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                           emptyAluIn))
                  CrvfsmwbRead_load (rc_a1QEB, f3_a1Qx0, addr_a1Qx1)
                    -> (Cforever_rvfsm
                          (v_a1QwQ, rc_a1QEB, ((mkDatI f3_a1Qx0) addr_a1Qx1) v_a1QQK), 
                        ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                         emptyAluIn))
          else
              (Cwhile_rvfsm (v_a1QwQ, adr_a1QwD, sel_a1QwE, c_a1QDT), 
               ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cwhile_rvfsm0 (v_a1Qxr, adr_a1Qxd, dat_a1Qxe, sel_a1Qxf))
        (wb, _, _)
        = if wbAck wb then
              (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), v_a1Qxr, 4), 
               (((writeWishboneOut adr_a1Qxd) dat_a1Qxe) sel_a1Qxf, 
                emptyRegFileIn, emptyAluIn))
          else
              (Cwhile_rvfsm0 (v_a1Qxr, adr_a1Qxd, dat_a1Qxe, sel_a1Qxf), 
               (((writeWishboneOut adr_a1Qxd) dat_a1Qxe) sel_a1Qxf, 
                emptyRegFileIn, emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm (v_a1QAn, instr_a1QA5, r_a1QAm))
        (_, _, _)
        = (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), v_a1QAn, 4), 
           (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QAm, 
            emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm0 (pc_a1QzL, instr_a1QA5, r_a1QAO))
        (wb, rf, _)
        = let f3_a1QFE = bitCoerce $ iFunct3 instr_a1QA5 in
          let sel_a1QFP = (mkSel f3_a1QFE) r_a1QAO in
          let dat_a1QFO = ((mkDatO f3_a1QFE) r_a1QAO) (rfRS2V rf) in
          let adr_a1QFN = mkAdr r_a1QAO
          in
            if wbAck wb then
                (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
                 (((writeWishboneOut adr_a1QFN) dat_a1QFO) sel_a1QFP, 
                  emptyRegFileIn, emptyAluIn))
            else
                (Cwhile_rvfsm0 (pc_a1QzL, adr_a1QFN, dat_a1QFO, sel_a1QFP), 
                 (((writeWishboneOut adr_a1QFN) dat_a1QFO) sel_a1QFP, 
                  emptyRegFileIn, emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm1 (pc_a1QzL, instr_a1QA5, r_a1QBj))
        (_, _, _)
        = (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
           (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QBj, 
            emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm2 (pc_a1QzL, instr_a1QA5, r_a1QBG))
        (_, _, _)
        = (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
           (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QBG, 
            emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm3 (pc_a1QzL, instr_a1QA5, imm_a1QA4, aluInstr_a1QA6,
                          r_a1QA8))
        (_, rf, ali)
        = case iOpcode instr_a1QA5 of
            OLoad
              -> let f3_a1QGL = bitCoerce $ iFunct3 instr_a1QA5
                 in
                   (Cwhile_rvfsm
                      (pc_a1QzL, mkAdr ali, (mkSel f3_a1QGL) ali,
                       CrvfsmwbRead_load (instr_a1QA5, f3_a1QGL, ali)), 
                    (emptyWishboneOut, emptyRegFileIn, 
                     ((doAluIn aiAdd) (rfRS1V rf)) imm_a1QA4))
            OStore
              -> (Cforever_rvfsm0 (pc_a1QzL, instr_a1QA5, ali), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aiAdd) (rfRS1V rf)) imm_a1QA4))
            OOp
              -> (Cforever_rvfsm1 (pc_a1QzL, instr_a1QA5, ali), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aluInstr_a1QA6) (rfRS1V rf)) (rfRS2V rf)))
            OOpImm
              -> (Cforever_rvfsm2 (pc_a1QzL, instr_a1QA5, ali), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aluInstr_a1QA6) (rfRS1V rf)) imm_a1QA4))
            OBranch
              -> (CuseAlu_rvfsm
                    (CrvfsmuseAlu_pc4 (), if ali == 0 then pc_a1QzL else r_a1QA8, 4), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aluInstr_a1QA6) (rfRS1V rf)) (rfRS2V rf)))
            OLui
              -> (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) imm_a1QA4, 
                   emptyAluIn))
            OJal
              -> (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), r_a1QA8, 4), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) pc_a1QzL, 
                   emptyAluIn))
            OJalr
              -> (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), ali, 4), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) pc_a1QzL, 
                   ((doAluIn aiAdd) (rfRS1V rf)) imm_a1QA4))
            OAuipc
              -> (CuseAlu_rvfsm (CrvfsmuseAlu_pc4 (), pc_a1QzL, 4), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QA8, 
                   emptyAluIn))


