module FSM.RiscV.YieldCtlDataManual(
    rvfsmManual
) where

import Clash.Prelude
import FSM.RiscV.Arch
import FSM.RiscV.Alu
import FSM.RiscV.RegFile
import FSM.RiscV.Immediate
import FSM.RiscV.Wishbone

data FSMState_rvfsm_a1QRr a_a1QRt b_a1QRu c_a1QRv adr_a1QRw sel_a1QRx v_a1QRy c_a1QRz adr_a1QRA dat_a1QRB sel_a1QRC v_a1QRD instr_a1QRE r_a1QRF v_a1QRG pc_a1QRH instr_a1QRI r_a1QRJ pc_a1QRK instr_a1QRL r_a1QRM pc_a1QRN instr_a1QRO r_a1QRP pc_a1QRQ imm_a1QRR instr_a1QRS aluInstr_a1QRT r_a1QRU
  = CuseAlu_rvfsm (a_a1QRt, b_a1QRu, c_a1QRv) |
    Cwhile_rvfsm (adr_a1QRw, sel_a1QRx, v_a1QRy, c_a1QRz) |
    Cwhile_rvfsm0 (adr_a1QRA, dat_a1QRB, sel_a1QRC, v_a1QRD) |
    Cforever_rvfsm (instr_a1QRE, r_a1QRF, v_a1QRG) |
    Cforever_rvfsm0 (pc_a1QRH, instr_a1QRI, r_a1QRJ) |
    Cforever_rvfsm1 (pc_a1QRK, instr_a1QRL, r_a1QRM) |
    Cforever_rvfsm2 (pc_a1QRN, instr_a1QRO, r_a1QRP) |
    Cforever_rvfsm3 (pc_a1QRQ, imm_a1QRR, instr_a1QRS, aluInstr_a1QRT,
                     r_a1QRU)
  deriving (Show, Generic, NFDataX)
rvfsmManual ::
  HiddenClockResetEnable dom =>
  CpuWord
  -> Signal dom (WishboneIn, RegFileOut, AluOut)
     -> Signal dom (WishboneOut, RegFileIn, AluIn)
rvfsmManual startPC
  = (mealy fsmFunc_rvfsm_a1QRs) fsmInitState_rvfsm_a1QRq
  where
      fsmInitState_rvfsm_a1QRq
        = CuseAlu_rvfsm (startPC, 4, CrvfsmuseAlu19_a1QDV ())
      fsmFunc_rvfsm_a1QRs
        (CuseAlu_rvfsm (a_a1Qwv, b_a1Qww, c_a1QDK))
        (wb, _, ali)
        = let v_a1QQG = ((doAluIn aiAdd) a_a1Qwv) b_a1Qww
          in
            case c_a1QDK of
              CrvfsmuseAlu28_a1QE8 (r_a1QzR, imm_a1QA4, instr_a1QA5,
                                    aluInstr_a1QA6)
                -> (Cforever_rvfsm3
                      (r_a1QzR, imm_a1QA4, instr_a1QA5, aluInstr_a1QA6, ali), 
                    (emptyWishboneOut, 
                     (readRegFileIn (iRS1 instr_a1QA5)) (iRS2 instr_a1QA5), v_a1QQG))
              CrvfsmuseAlu19_a1QDV ()
                -> let sel_a1QQf = negate 1 in
                   let adr_a1QQe = mkAdr a_a1Qwv
                   in
                     if wbAck wb then
                         let r_a1QzY = wbDatI wb in
                         let imm_a1QA4 = immediate r_a1QzY in
                         let instr_a1QA5 = decodeInstr r_a1QzY
                         in
                           (CuseAlu_rvfsm
                              (a_a1Qwv, imm_a1QA4, 
                               CrvfsmuseAlu28_a1QE8
                                 (ali, imm_a1QA4, instr_a1QA5, 
                                  ((decodeAluInstr (iOpcode instr_a1QA5)) (iFunct3 instr_a1QA5))
                                    (iFunct7 instr_a1QA5))), 
                            ((readWishboneOut adr_a1QQe) sel_a1QQf, emptyRegFileIn, v_a1QQG))
                     else
                         (Cwhile_rvfsm
                            (adr_a1QQe, sel_a1QQf, a_a1Qwv, CrvfsmwbRead29_a1QE9 ali), 
                          ((readWishboneOut adr_a1QQe) sel_a1QQf, emptyRegFileIn, v_a1QQG))
      fsmFunc_rvfsm_a1QRs
        (Cwhile_rvfsm (adr_a1QwD, sel_a1QwE, v_a1QwQ, c_a1QDT))
        (wb, _, _)
        = if wbAck wb then
              let v_a1QQK = wbDatI wb
              in
                case c_a1QDT of
                  CrvfsmwbRead29_a1QE9 r_a1QzR
                    -> let imm_a1QA4 = immediate v_a1QQK in
                       let instr_a1QA5 = decodeInstr v_a1QQK
                       in
                         (CuseAlu_rvfsm
                            (v_a1QwQ, imm_a1QA4, 
                             CrvfsmuseAlu28_a1QE8
                               (r_a1QzR, imm_a1QA4, instr_a1QA5, 
                                ((decodeAluInstr (iOpcode instr_a1QA5)) (iFunct3 instr_a1QA5))
                                  (iFunct7 instr_a1QA5))), 
                          ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                           emptyAluIn))
                  CrvfsmwbRead16_a1QDO (rc_a1QEB, f3_a1Qx0, addr_a1Qx1)
                    -> (Cforever_rvfsm
                          (rc_a1QEB, ((mkDatI f3_a1Qx0) addr_a1Qx1) v_a1QQK, v_a1QwQ), 
                        ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                         emptyAluIn))
          else
              (Cwhile_rvfsm (adr_a1QwD, sel_a1QwE, v_a1QwQ, c_a1QDT), 
               ((readWishboneOut adr_a1QwD) sel_a1QwE, emptyRegFileIn, 
                emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cwhile_rvfsm0 (adr_a1Qxd, dat_a1Qxe, sel_a1Qxf, v_a1Qxr))
        (wb, _, _)
        = if wbAck wb then
              (CuseAlu_rvfsm (v_a1Qxr, 4, CrvfsmuseAlu19_a1QDV ()), 
               (((writeWishboneOut adr_a1Qxd) dat_a1Qxe) sel_a1Qxf, 
                emptyRegFileIn, emptyAluIn))
          else
              (Cwhile_rvfsm0 (adr_a1Qxd, dat_a1Qxe, sel_a1Qxf, v_a1Qxr), 
               (((writeWishboneOut adr_a1Qxd) dat_a1Qxe) sel_a1Qxf, 
                emptyRegFileIn, emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm (instr_a1QA5, r_a1QAm, v_a1QAn))
        (_, _, _)
        = (CuseAlu_rvfsm (v_a1QAn, 4, CrvfsmuseAlu19_a1QDV ()), 
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
                (CuseAlu_rvfsm (pc_a1QzL, 4, CrvfsmuseAlu19_a1QDV ()), 
                 (((writeWishboneOut adr_a1QFN) dat_a1QFO) sel_a1QFP, 
                  emptyRegFileIn, emptyAluIn))
            else
                (Cwhile_rvfsm0 (adr_a1QFN, dat_a1QFO, sel_a1QFP, pc_a1QzL), 
                 (((writeWishboneOut adr_a1QFN) dat_a1QFO) sel_a1QFP, 
                  emptyRegFileIn, emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm1 (pc_a1QzL, instr_a1QA5, r_a1QBj))
        (_, _, _)
        = (CuseAlu_rvfsm (pc_a1QzL, 4, CrvfsmuseAlu19_a1QDV ()), 
           (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QBj, 
            emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm2 (pc_a1QzL, instr_a1QA5, r_a1QBG))
        (_, _, _)
        = (CuseAlu_rvfsm (pc_a1QzL, 4, CrvfsmuseAlu19_a1QDV ()), 
           (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QBG, 
            emptyAluIn))
      fsmFunc_rvfsm_a1QRs
        (Cforever_rvfsm3 (pc_a1QzL, imm_a1QA4, instr_a1QA5, aluInstr_a1QA6,
                          r_a1QA8))
        (_, rf, ali)
        = case iOpcode instr_a1QA5 of
            OLoad
              -> let f3_a1QGL = bitCoerce $ iFunct3 instr_a1QA5
                 in
                   (Cwhile_rvfsm
                      (mkAdr ali, (mkSel f3_a1QGL) ali, pc_a1QzL, 
                       CrvfsmwbRead16_a1QDO (instr_a1QA5, f3_a1QGL, ali)), 
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
                    (if ali == 0 then pc_a1QzL else r_a1QA8, 4, 
                     CrvfsmuseAlu19_a1QDV ()), 
                  (emptyWishboneOut, emptyRegFileIn, 
                   ((doAluIn aluInstr_a1QA6) (rfRS1V rf)) (rfRS2V rf)))
            OLui
              -> (CuseAlu_rvfsm (pc_a1QzL, 4, CrvfsmuseAlu19_a1QDV ()), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) imm_a1QA4, 
                   emptyAluIn))
            OJal
              -> (CuseAlu_rvfsm (r_a1QA8, 4, CrvfsmuseAlu19_a1QDV ()), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) pc_a1QzL, 
                   emptyAluIn))
            OJalr
              -> (CuseAlu_rvfsm (ali, 4, CrvfsmuseAlu19_a1QDV ()), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) pc_a1QzL, 
                   ((doAluIn aiAdd) (rfRS1V rf)) imm_a1QA4))
            OAuipc
              -> (CuseAlu_rvfsm (pc_a1QzL, 4, CrvfsmuseAlu19_a1QDV ()), 
                  (emptyWishboneOut, (writeRegFileIn (iRD instr_a1QA5)) r_a1QA8, 
                   emptyAluIn))
data CTrvfsmwbRead13_a1QDG rc_a1QEL f3_a1Qx0 addr_a1Qx1 r_a1QzR
  = CrvfsmwbRead16_a1QDO (rc_a1QEL, f3_a1Qx0, addr_a1Qx1) |
    CrvfsmwbRead29_a1QE9 r_a1QzR
  deriving (Show, Generic, NFDataX)
data CTrvfsmuseAlu14_a1QDI r_a1QzR imm_a1QA4 instr_a1QA5 aluInstr_a1QA6
  = CrvfsmuseAlu19_a1QDV () |
    CrvfsmuseAlu28_a1QE8 (r_a1QzR, imm_a1QA4, instr_a1QA5,
                          aluInstr_a1QA6)
  deriving (Show, Generic, NFDataX)

