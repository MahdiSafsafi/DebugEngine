//
// *************************************************************************** //
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
// *************************************************************************** //
//
//
// *************************************************************************** //
// UnivDisasm library.
//
// This file is a part of UnivDisasm library.
//
// https://github.com/MahdiSafsafi/UnivDisasm
//
// The Original Code is GROUPS.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_GROUP_1(PInst: PInstruction);
const
  GRP80Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ADD_Eb_Ib, { }
    Decode_GP_OR_Eb_Ib, { }
    Decode_GP_ADC_Eb_Ib, { }
    Decode_GP_SBB_Eb_Ib, { }
    Decode_GP_AND_Eb_Ib, { }
    Decode_GP_SUB_Eb_Ib, { }
    Decode_GP_XOR_Eb_Ib, { }
    Decode_GP_CMP_Eb_Ib { }
    );
  GRP81Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ADD_Ev_Iz, { }
    Decode_GP_OR_Ev_Iz, { }
    Decode_GP_ADC_Ev_Iz, { }
    Decode_GP_SBB_Ev_Iz, { }
    Decode_GP_AND_Ev_Iz, { }
    Decode_GP_SUB_Ev_Iz, { }
    Decode_GP_XOR_Ev_Iz, { }
    Decode_GP_CMP_Ev_Iz { }
    );
  GRP82Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ADD_Eb_Ib, { }
    Decode_GP_OR_Eb_Ib, { }
    Decode_GP_ADC_Eb_Ib, { }
    Decode_GP_SBB_Eb_Ib, { }
    Decode_GP_AND_Eb_Ib, { }
    Decode_GP_SUB_Eb_Ib, { }
    Decode_GP_XOR_Eb_Ib, { }
    Decode_GP_CMP_Eb_Ib { } );
  GRP83Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ADD_Ev_Ib, { }
    Decode_GP_OR_Ev_Ib, { }
    Decode_GP_ADC_Ev_Ib, { }
    Decode_GP_SBB_Ev_Ib, { }
    Decode_GP_AND_Ev_Ib, { }
    Decode_GP_SUB_Ev_Ib, { }
    Decode_GP_XOR_Ev_Ib, { }
    Decode_GP_CMP_Ev_Ib { } );
var
  P: PByte;
begin
  PInst^.SetGroup(GF_GROUP_1);
  P := PInst^.NextInst;
  Dec(P);
  DecodeModRm(PInst);
  case P^ of
    $80: GRP80Array[PInst^.ModRm.Value.Reg](PInst);
    $81: GRP81Array[PInst^.ModRm.Value.Reg](PInst);
    $82: GRP82Array[PInst^.ModRm.Value.Reg](PInst);
    $83: GRP83Array[PInst^.ModRm.Value.Reg](PInst);
  end;
end;

procedure Decode_GROUP_10(PInst: PInstruction);
begin
  PInst^.SetGroup(GF_GROUP_10);
  DecodeModRm(PInst);
  Decode_GP_UD2_void(PInst);
end;

procedure Decode_GROUP_11(PInst: PInstruction);
var
  P: PByte;
  ModRm: TByteModRm;
begin
  PInst^.SetGroup(GF_GROUP_11);
  P := PInst^.NextInst;
  Dec(P);
  DecodeModRm(PInst);
  ModRm := PInst^.ModRm.Value;
  case P^ of
    $C6:
      begin
        if ModRm = $F8 then
          Decode_TSX_XABORT_Ib(PInst)
        else if (ModRm.Reg = $00) then
          Decode_GP_MOV_Eb_Ib(PInst);
      end;
    $C7:
      begin
        if ModRm = $F8 then
          Decode_TSX_XBEGIN_Jz(PInst)
        else if (ModRm.Reg = $00) then
          Decode_GP_MOV_Ev_Iz(PInst);
      end;
  end;
end;

procedure Decode_GROUP_12(PInst: PInstruction);
var
  ModRm: TByteModRm;
begin
  PInst^.SetGroup(GF_GROUP_12);
  DecodeModRm(PInst);
  ModRm := PInst^.ModRm.Value;
  if PInst^.Encoding = ENC_EVEX then
  begin
    case ModRm.Reg of
      $02: Decode_AVX512BW_VL_VPSRLW_Hn_K_z_Wn_Ib(PInst);
      $04: Decode_AVX512BW_VL_VPSRAW_Hn_K_z_Wn_Ib(PInst);
      $06: Decode_AVX512BW_VL_VPSLLW_Hn_K_z_Wn_Ib(PInst);
    end;
  end
  else if (PInst^.InternalData.MandatoryPrefixes = MND_PRF_66) then
  begin
    if (PInst^.Encoding = ENC_VEX) then
    begin
      if (PInst^.Fields.VL = VL128) then
      begin
        case ModRm.Reg of
          $02: Decode_AVX_VPSRLW_Ho_Uo_Ib(PInst);
          $04: Decode_AVX_VPSRAW_Ho_Uo_Ib(PInst);
          $06: Decode_AVX_VPSLLW_Ho_Uo_Ib(PInst);
        end;
      end
      else if (PInst^.Fields.VL = VL256) then
      begin
        case ModRm.Reg of
          $02: Decode_AVX2_VPSRLW_Hy_Uy_Ib(PInst);
          $04: Decode_AVX2_VPSRAW_Hy_Uy_Ib(PInst);
          $06: Decode_AVX2_VPSLLW_Hy_Uy_Ib(PInst);
        end;
      end;
    end
    else
    begin
      case ModRm.Reg of
        $02: Decode_SSE2_PSRLW_Uo_Ib(PInst);
        $04: Decode_SSE2_PSRAW_Uo_Ib(PInst);
        $06: Decode_SSE2_PSLLW_Uo_Ib(PInst);
      end;
    end;
  end
  else
  begin
    case ModRm.Reg of
      $02: Decode_MMX_PSRLW_Nq_Ib(PInst);
      $04: Decode_MMX_PSRAW_Nq_Ib(PInst);
      $06: Decode_MMX_PSLLW_Nq_Ib(PInst);
    end;
  end;
end;

procedure Decode_GROUP_13(PInst: PInstruction);
var
  ModRm: TByteModRm;
begin
  PInst^.SetGroup(GF_GROUP_13);
  DecodeModRm(PInst);
  ModRm := PInst^.ModRm.Value;
  if PInst^.Encoding = ENC_EVEX then
  begin
    case ModRm.Reg of
      $00:
        begin
          if PInst^.Fields.W then
            Decode_AVX512F_VL_VPRORQ_Hn_K_z_B64_Wn_Ib(PInst)
          else
            Decode_AVX512F_VL_VPRORD_Hn_K_z_B32_Wn_Ib(PInst);
        end;
      $01:
        begin
          if PInst^.Fields.W then
            Decode_AVX512F_VL_VPROLQ_Hn_K_z_B64_Wn_Ib(PInst)
          else
            Decode_AVX512F_VL_VPROLD_Hn_K_z_B32_Wn_Ib(PInst);
        end;
      $02: Decode_AVX512F_VL_VPSRLD_Hn_K_z_B32_Wn_Ib(PInst);
      $04: Decode_AVX512F_VL_VPSRAD_Hn_K_z_B32_Wn_Ib(PInst);
      $06: Decode_AVX512F_VL_VPSLLD_Hn_K_z_B32_Wn_Ib(PInst);
    end;
  end
  else if (PInst^.InternalData.MandatoryPrefixes = MND_PRF_66) then
  begin
    if (PInst^.Encoding = ENC_VEX) then
    begin
      if (PInst^.Fields.VL = VL128) then
      begin
        case ModRm.Reg of
          $02: Decode_AVX_VPSRLD_Ho_Uo_Ib(PInst);
          $04: Decode_AVX_VPSRAD_Ho_Uo_Ib(PInst);
          $06: Decode_AVX_VPSLLD_Ho_Uo_Ib(PInst);
        end;
      end
      else if (PInst^.Fields.VL = VL256) then
      begin
        case ModRm.Reg of
          $02: Decode_AVX2_VPSRLD_Hy_Uy_Ib(PInst);
          $04: Decode_AVX2_VPSRAD_Hy_Uy_Ib(PInst);
          $06: Decode_AVX2_VPSLLD_Hy_Uy_Ib(PInst);
        end;
      end;
    end
    else
    begin
      case ModRm.Reg of
        $02: Decode_SSE2_PSRLD_Uo_Ib(PInst);
        $04: Decode_SSE2_PSRAD_Uo_Ib(PInst);
        $06: Decode_SSE2_PSLLD_Uo_Ib(PInst);
      end;
    end;
  end
  else
  begin
    case ModRm.Reg of
      $02: Decode_MMX_PSRLD_Nq_Ib(PInst);
      $04: Decode_MMX_PSRAD_Nq_Ib(PInst);
      $06: Decode_MMX_PSLLD_Nq_Ib(PInst);
    end;
  end;
end;

procedure Decode_GROUP_14(PInst: PInstruction);
var
  ModRm: TByteModRm;
begin
  PInst^.SetGroup(GF_GROUP_14);
  DecodeModRm(PInst);
  ModRm := PInst^.ModRm.Value;
  if PInst^.Encoding = ENC_EVEX then
  begin
    { EVEX }
    case ModRm.Reg of
      $02:
        begin
          if PInst^.Fields.W then
            Decode_AVX512F_VL_VPSRLQ_Hn_K_z_B64_Wn_Ib(PInst);
        end;
      $06:
        begin
          if PInst^.Fields.W then
            Decode_AVX512F_VL_VPSLLQ_Hn_K_z_B64_Wn_Ib(PInst);
        end;
      $03: Decode_AVX512BW_VL_VPSRLDQ_Hn_Wn_Ib(PInst);
      $07: Decode_AVX512BW_VL_VPSLLDQ_Hn_Wn_Ib(PInst);
    end;
  end
  else
  begin
    { NO EVEX }
    if PInst^.InternalData.MandatoryPrefixes and MND_PRF_66 <> 0 then
    begin
      { Mandatory 66 prefix }
      if PInst^.Encoding = ENC_VEX then
      begin
        { VEX + 66 }
        if (PInst^.Fields.VL = VL128) then
        begin
          case ModRm.Reg of
            $02: Decode_AVX_VPSRLQ_Ho_Uo_Ib(PInst);
            $03: Decode_AVX_VPSRLDQ_Ho_Uo_Ib(PInst);
            $06: Decode_AVX_VPSLLQ_Ho_Uo_Ib(PInst);
            $07: Decode_AVX_VPSLLDQ_Ho_Uo_Ib(PInst);
          end;
        end
        else if (PInst^.Fields.VL = VL256) then
        begin
          case ModRm.Reg of
            $02: Decode_AVX2_VPSRLQ_Hy_Uy_Ib(PInst);
            $03: Decode_AVX2_VPSRLDQ_Hy_Uy_Ib(PInst);
            $06: Decode_AVX2_VPSLLQ_Hy_Uy_Ib(PInst);
            $07: Decode_AVX2_VPSLLDQ_Hy_Uy_Ib(PInst);
          end;
        end;
      end
      else
      begin
        { NO VEX }
        case ModRm.Reg of
          $02: Decode_SSE2_PSRLQ_Uo_Ib(PInst);
          $03: Decode_SSE2_PSRLDQ_Uo_Ib(PInst);
          $06: Decode_SSE2_PSLLQ_Uo_Ib(PInst);
          $07: Decode_SSE2_PSLLDQ_Uo_Ib(PInst);
        end;
      end;
    end
    else
    begin
      { NO 66 }
      case ModRm.Reg of
        $02: Decode_MMX_PSRLQ_Nq_Ib(PInst);
        $06: Decode_MMX_PSLLQ_Nq_Ib(PInst);
      end;
    end;
  end;
end;

procedure Decode_GRP15_MEM_6(PInst: PInstruction);
begin
  if PInst^.InternalData.MandatoryPrefixes and MND_PRF_66 <> 0 then
    Decode_INSX_CLFLUSHOPT_M(PInst)
  else
    Decode_SSE2_CLFLUSH_M(PInst);
end;

procedure Decode_GRP15_MEM_7(PInst: PInstruction);
begin
  if PInst^.InternalData.MandatoryPrefixes and MND_PRF_66 <> 0 then
    Decode_INSX_CLWB_M(PInst)
  else
    Decode_SYSTEM_XSAVEOPT_M(PInst);
end;

procedure Decode_GROUP_15(PInst: PInstruction);

const
  GRP15_AE_MEM_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_FPU_FXSAVE_M, { }
    Decode_FPU_FXRSTOR_M, { }
    Decode_SSE_VLDMXCSR_Md, { }
    Decode_SSE_VSTMXCSR_Md, { }
    Decode_SYSTEM_XSAVE_M, { }
    Decode_SYSTEM_XRSTOR_M, { }
    Decode_GRP15_MEM_6, { }
    Decode_GRP15_MEM_7 { } );
var
  P: PByte;
begin
  PInst^.SetGroup(GF_GROUP_15);
  P := PInst^.NextInst;
  Dec(P);
  DecodeModRm(PInst);
  case P^ of
    $AE:
      begin
        if PInst^.ModRm.Value.IsMem then
        begin
          GRP15_AE_MEM_Array[PInst^.ModRm.Value.Reg](PInst);
        end
        else
        begin
          case PInst^.ModRm.Value.Reg of
            $00:
              begin
                if PInst^.InternalData.MandatoryPrefixes and MND_PRF_F3 <> 0 then
                  Decode_SYSTEM_RDFSBASE_Ry(PInst);
              end;
            $01:
              begin
                if PInst^.InternalData.MandatoryPrefixes and MND_PRF_F3 <> 0 then
                  Decode_SYSTEM_RDGSBASE_Ry(PInst);
              end;
            $02:
              begin
                if PInst^.InternalData.MandatoryPrefixes and MND_PRF_F3 <> 0 then
                  Decode_SYSTEM_WRFSBASE_Ry(PInst);
              end;
            $03:
              begin
                if PInst^.InternalData.MandatoryPrefixes and MND_PRF_F3 <> 0 then
                  Decode_SYSTEM_WRGSBASE_Ry(PInst);
              end;
            $05:
              begin
                Decode_MEM_SSE2_LFENCE_void(PInst);
              end;
            $06:
              begin
                Decode_MEM_SSE2_MFENCE_void(PInst);
              end;
            $07:
              begin
                if PInst^.ModRm.Value = $F8 then
                  Decode_INSX_PCOMMIT_void(PInst)
                else
                  Decode_MEM_SSE_SFENCE_void(PInst);
              end;
          end;
        end;
      end;
  end;
end;

procedure Decode_GROUP_16(PInst: PInstruction);
const
  GRP16_18_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_MEM_SSE_PREFETCHNTA_M, { }
    Decode_MEM_SSE_PREFETCHT0_M, { }
    Decode_MEM_SSE_PREFETCHT1_M, { }
    Decode_MEM_SSE_PREFETCHT2_M, { }
    Decode_INSX_HINT_NOP_Ev, { }
    Decode_INSX_HINT_NOP_Ev, { }
    Decode_INSX_HINT_NOP_Ev, { }
    Decode_INSX_HINT_NOP_Ev { } );
var
  P: PByte;
begin
  PInst^.SetGroup(GF_GROUP_16);
  P := PInst^.NextInst;
  Dec(P);
  DecodeModRm(PInst);
  case P^ of
    $18: GRP16_18_Array[PInst^.ModRm.Value.Reg](PInst);
    $19: Decode_INSX_HINT_NOP_Ev(PInst);
    $1A:
      begin
        case PInst^.InternalData.MandatoryPrefixes of
          MND_PRF_NA: Decode_MPX_BNDLDX_rB_Mib(PInst);
          MND_PRF_F2: Decode_MPX_BNDCU_rB_Ey(PInst);
          MND_PRF_F3: Decode_MPX_BNDCL_rB_Ey(PInst);
          MND_PRF_66:
            begin
              if PInst^.ModRm.Value.IsMem then
                Decode_MPX_BNDMOV_rB_M(PInst)
              else
                Decode_MPX_BNDMOV_rB_mB(PInst)
            end;
        end;
      end;
    $1B:
      begin
        case PInst^.InternalData.MandatoryPrefixes of
          MND_PRF_NA: Decode_MPX_BNDSTX_Mib_rB(PInst);
          MND_PRF_F2: Decode_MPX_BNDCN_rB_Ey(PInst);
          MND_PRF_F3: Decode_MPX_BNDMK_rB_My(PInst);
          MND_PRF_66:
            begin
              if PInst^.ModRm.Value.IsMem then
                Decode_MPX_BNDMOV_M_rB(PInst)
              else
                Decode_MPX_BNDMOV_mB_rB(PInst)
            end;
        end;
      end;
  end;
end;

procedure Decode_GROUP_17(PInst: PInstruction);
const
  GRP17_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_BMI_BLSR_By_Ey, { }
    Decode_BMI_BLSMSK_By_Ey, { }
    Decode_BMI_BLSI_By_Ey, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID { } );

begin
  PInst^.SetGroup(GF_GROUP_17);
  DecodeModRm(PInst);
  GRP17_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_GROUP_18(PInst: PInstruction);
const
  GRP18_C6_W0_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_AVX512PF_VSIB_VGATHERPF0DPS_Md_z_K_not_0, { }
    Decode_AVX512PF_VSIB_VGATHERPF1DPS_Md_z_K_not_0, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_AVX512PF_VSIB_VSCATTERPF0DPS_Md_z_K_not_0, { }
    Decode_AVX512PF_VSIB_VSCATTERPF1DPS_Md_z_K_not_0, { }
    Decode_INVALID { }
    );
  GRP18_C6_W1_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_AVX512PF_VSIB_VGATHERPF0DPD_Mq_y_K_not_0, { }
    Decode_AVX512PF_VSIB_VGATHERPF1DPD_Mq_y_K_not_0, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_AVX512PF_VSIB_VSCATTERPF0DPD_Mq_y_K_not_0, { }
    Decode_AVX512PF_VSIB_VSCATTERPF1DPD_Mq_y_K_not_0, { }
    Decode_INVALID { }
    );
  GRP18_C7_W0_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_AVX512PF_VSIB_VGATHERPF0QPS_Md_z_K_not_0, { }
    Decode_AVX512PF_VSIB_VGATHERPF1QPS_Md_z_K_not_0, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_AVX512PF_VSIB_VSCATTERPF0QPS_Md_z_K_not_0, { }
    Decode_AVX512PF_VSIB_VSCATTERPF1QPS_Md_z_K_not_0, { }
    Decode_INVALID { } );
  GRP18_C7_W1_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_AVX512PF_VSIB_VGATHERPF0QPD_Mq_z_K_not_0, { }
    Decode_AVX512PF_VSIB_VGATHERPF1QPD_Mq_z_K_not_0, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_AVX512PF_VSIB_VSCATTERPF0QPD_Mq_z_K_not_0, { }
    Decode_AVX512PF_VSIB_VSCATTERPF1QPD_Mq_z_K_not_0, { }
    Decode_INVALID { } );

var
  P: PByte;
begin
  PInst^.SetGroup(GF_GROUP_18);
  P := PInst^.NextInst;
  Dec(P);
  DecodeModRm(PInst);
  case P^ of
    $C6:
      begin
        if PInst^.Fields.W then
          GRP18_C6_W1_Array[PInst^.ModRm.Value.Reg](PInst)
        else
          GRP18_C6_W0_Array[PInst^.ModRm.Value.Reg](PInst)
      end;
    $C7:
      begin
        if PInst^.Fields.W then
          GRP18_C7_W1_Array[PInst^.ModRm.Value.Reg](PInst)
        else
          GRP18_C7_W0_Array[PInst^.ModRm.Value.Reg](PInst);
      end;
  end;
end;

procedure Decode_GROUP_1A(PInst: PInstruction);
begin
  PInst^.SetGroup(GF_GROUP_1A);
  if PInst^.Vendor = VENDOR_AMD then
  begin
    if (PInst^.NextInst^ and $1F) < $08 then
      Decode_GP_POP_Ev_D64(PInst)
    else
      Decode_PREFIXES_XOP_void(PInst);
  end
  else
  begin
    DecodeModRm(PInst);
    if (PInst^.ModRm.Value.Reg = $00) then
      Decode_GP_POP_Ev_D64(PInst);
  end;
end;

procedure Decode_GROUP_2(PInst: PInstruction);
const
  GRP2_C0_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ROL_Eb_Ib, { }
    Decode_GP_ROR_Eb_Ib, { }
    Decode_GP_RCL_Eb_Ib, { }
    Decode_GP_RCR_Eb_Ib, { }
    Decode_GP_SHL_Eb_Ib, { }
    Decode_GP_SHR_Eb_Ib, { }
    Decode_GP_SAL_Eb_Ib, { }
    Decode_GP_SAR_Eb_Ib { }
    );

  GRP2_C1_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ROL_Ev_Ib, { }
    Decode_GP_ROR_Ev_Ib, { }
    Decode_GP_RCL_Ev_Ib, { }
    Decode_GP_RCR_Ev_Ib, { }
    Decode_GP_SHL_Ev_Ib, { }
    Decode_GP_SHR_Ev_Ib, { }
    Decode_GP_SAL_Ev_Ib, { }
    Decode_GP_SAR_Ev_Ib { }
    );

  GRP2_D0_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ROL_Eb_1, { }
    Decode_GP_ROR_Eb_1, { }
    Decode_GP_RCL_Eb_1, { }
    Decode_GP_RCR_Eb_1, { }
    Decode_GP_SHL_Eb_1, { }
    Decode_GP_SHR_Eb_1, { }
    Decode_GP_SAL_Eb_1, { }
    Decode_GP_SAR_Eb_1 { }
    );

  GRP2_D1_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ROL_Ev_1, { }
    Decode_GP_ROR_Ev_1, { }
    Decode_GP_RCL_Ev_1, { }
    Decode_GP_RCR_Ev_1, { }
    Decode_GP_SHL_Ev_1, { }
    Decode_GP_SHR_Ev_1, { }
    Decode_GP_SAL_Ev_1, { }
    Decode_GP_SAR_Ev_1 { }
    );

  GRP2_D2_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ROL_Eb_CL, { }
    Decode_GP_ROR_Eb_CL, { }
    Decode_GP_RCL_Eb_CL, { }
    Decode_GP_RCR_Eb_CL, { }
    Decode_GP_SHL_Eb_CL, { }
    Decode_GP_SHR_Eb_CL, { }
    Decode_GP_SAL_Eb_CL, { }
    Decode_GP_SAR_Eb_CL { }
    );

  GRP2_D3_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_ROL_Ev_CL, { }
    Decode_GP_ROR_Ev_CL, { }
    Decode_GP_RCL_Ev_CL, { }
    Decode_GP_RCR_Ev_CL, { }
    Decode_GP_SHL_Ev_CL, { }
    Decode_GP_SHR_Ev_CL, { }
    Decode_GP_SAL_Ev_CL, { }
    Decode_GP_SAR_Ev_CL { }
    );
var
  P: PByte;
begin
  PInst^.SetGroup(GF_GROUP_2);
  P := PInst^.NextInst;
  Dec(P);
  DecodeModRm(PInst);
  case P^ of
    $C0: GRP2_C0_Array[PInst^.ModRm.Value.Reg](PInst);
    $C1: GRP2_C1_Array[PInst^.ModRm.Value.Reg](PInst);
    $D0: GRP2_D0_Array[PInst^.ModRm.Value.Reg](PInst);
    $D1: GRP2_D1_Array[PInst^.ModRm.Value.Reg](PInst);
    $D2: GRP2_D2_Array[PInst^.ModRm.Value.Reg](PInst);
    $D3: GRP2_D3_Array[PInst^.ModRm.Value.Reg](PInst);
  end;
end;

procedure Decode_GROUP_3(PInst: PInstruction);
const
  GRP3_F6_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_TEST_Eb_Ib, { }
    Decode_GP_TEST_Eb_Ib, { }
    Decode_GP_NOT_Eb, { }
    Decode_GP_NEG_Eb, { }
    Decode_GP_MUL_Eb, { }
    Decode_GP_IMUL_Eb, { }
    Decode_GP_DIV_Eb, { }
    Decode_GP_IDIV_Eb { }
    );

  GRP3_F7_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_TEST_Ev_Iz, { }
    Decode_GP_TEST_Ev_Iz, { }
    Decode_GP_NOT_Ev, { }
    Decode_GP_NEG_Ev, { }
    Decode_GP_MUL_Ev, { }
    Decode_GP_IMUL_Ev, { }
    Decode_GP_DIV_Ev, { }
    Decode_GP_IDIV_Ev { }
    );
var
  P: PByte;
begin
  PInst^.SetGroup(GF_GROUP_3);
  P := PInst^.NextInst;
  Dec(P);
  DecodeModRm(PInst);
  case P^ of
    $F6: GRP3_F6_Array[PInst^.ModRm.Value.Reg](PInst);
    $F7: GRP3_F7_Array[PInst^.ModRm.Value.Reg](PInst);
  end;
end;

procedure Decode_GROUP_4(PInst: PInstruction);
const
  GRP4_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_INC_Eb, Decode_GP_DEC_Eb, Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID { }
    );
begin
  PInst^.SetGroup(GF_GROUP_4);
  DecodeModRm(PInst);
  GRP4_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_GROUP_5(PInst: PInstruction);
const
  GRP5_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_GP_INC_Ev, { }
    Decode_GP_DEC_Ev, { }
    Decode_GP_CALL_Ev_Df64, { }
    Decode_GP_CALL_Mp_w_z, { }
    Decode_GP_JMP_Ev_Df64, { }
    Decode_GP_JMP_Mp_w_z, { }
    Decode_GP_PUSH_Ev_D64, { }
    Decode_INVALID { }
    );
begin
  PInst^.SetGroup(GF_GROUP_5);
  DecodeModRm(PInst);
  GRP5_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_GROUP_6(PInst: PInstruction);
const
  GRP6_MEM_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_SYSTEM_SLDT_Mw, { }
    Decode_SYSTEM_STR_Mw, { }
    Decode_SYSTEM_LLDT_Mw, { }
    Decode_SYSTEM_LTR_Mw, { }
    Decode_SYSTEM_VERR_Mw, { }
    Decode_SYSTEM_VERW_Mw, { }
    Decode_SYSTEM_JMPE_Ev, { }
    Decode_INVALID { }
    );

  GRP6_REG_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_SYSTEM_SLDT_Rv, { }
    Decode_SYSTEM_STR_Rv, { }
    Decode_SYSTEM_LLDT_Rv, { }
    Decode_SYSTEM_LTR_Rv, { }
    Decode_SYSTEM_VERR_Rv, { }
    Decode_SYSTEM_VERW_Rv, { }
    Decode_SYSTEM_JMPE_Ev, { }
    Decode_INVALID { }
    );
begin
  PInst^.SetGroup(GF_GROUP_6);
  DecodeModRm(PInst);
  if PInst^.ModRm.Value.IsMem then
    GRP6_MEM_Array[PInst^.ModRm.Value.Reg](PInst)
  else
    GRP6_REG_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_GROUP_7(PInst: PInstruction);
const
  GRP7_MEM_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_SYSTEM_SGDT_Mp_w_y_F64, { }
    Decode_SYSTEM_SIDT_Mp_w_y_F64, { }
    Decode_SYSTEM_LGDT_Mp_w_y_F64, { }
    Decode_SYSTEM_LIDT_Mp_w_y_F64, { }
    Decode_SYSTEM_SMSW_Mw, { }
    Decode_INVALID, { }
    Decode_SYSTEM_LMSW_Mw, { }
    Decode_SYSTEM_INVLPG_M { }
    );

  GRP7_REG_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_SYSTEM_SMSW_Rv, { }
    Decode_INVALID, { }
    Decode_SYSTEM_LMSW_Rv, { }
    Decode_INVALID { }
    );
var
  P: PByte;
begin
  PInst^.SetGroup(GF_GROUP_7);
  P := PInst^.NextInst;
  DecodeModRm(PInst);
  case P^ of
    $C1: Decode_VME_VMCALL_void(PInst);
    $C2: Decode_VME_VMLAUNCH_void(PInst);
    $C3: Decode_VME_VMRESUME_void(PInst);
    $C4: Decode_VME_VMXOFF_void(PInst);
    $C8: Decode_SSE3_MONITOR_void(PInst);
    $C9: Decode_SSE3_MWAIT_void(PInst);
    $CA: Decode_SYSTEM_CLAC_void(PInst);
    $CB: Decode_SYSTEM_STAC_void(PInst);
    $CF: Decode_INSX_ENCLS_void(PInst);
    $D0: Decode_SYSTEM_XGETBV_void(PInst);
    $D1: Decode_SYSTEM_XSETBV_void(PInst);
    $D4: Decode_VME_VMFUNC_void(PInst);
    $D5: Decode_TSX_XEND_void(PInst);
    $D6: Decode_TSX_XTEST_void(PInst);
    $D7: Decode_INSX_ENCLU_void(PInst);
    $D8: Decode_VME_VMRUN_void(PInst);
    $D9: Decode_VME_VMMCALL_void(PInst);
    $DA: Decode_VME_VMLOAD_void(PInst);
    $DB: Decode_VME_VMSAVE_void(PInst);
    $DC: Decode_INSX_STGI_void(PInst);
    $DD: Decode_INSX_CLGI_void(PInst);
    $DE: Decode_INSX_SKINIT_void(PInst);
    $DF: Decode_INSX_INVLPGA_void(PInst);
    $EE: Decode_INSX_RDPKRU_void(PInst);
    $EF: Decode_INSX_WRPKRU_void(PInst);
    $F8: Decode_GP_SWAPGS_void(PInst);
    $F9: Decode_SYSTEM_RDTSCP_void(PInst);
    $FA: Decode_INSX_MONITORX_void(PInst);
    $FB: Decode_INSX_MWAITX_void(PInst);
    $FC: Decode_INSX_CLZERO_void(PInst);
  else
    begin
      if PInst^.ModRm.Value.IsMem then
        GRP7_MEM_Array[PInst^.ModRm.Value.Reg](PInst)
      else
        GRP7_REG_Array[PInst^.ModRm.Value.Reg](PInst);
    end;
  end;
end;

procedure Decode_GROUP_8(PInst: PInstruction);
const
  GRP8_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_GP_BT_Ev_Ib, { }
    Decode_GP_BTS_Ev_Ib, { }
    Decode_GP_BTR_Ev_Ib, { }
    Decode_GP_BTC_Ev_Ib { }
    );
begin
  PInst^.SetGroup(GF_GROUP_8);
  DecodeModRm(PInst);
  GRP8_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_GROUP_9(PInst: PInstruction);
begin
  PInst^.SetGroup(GF_GROUP_9);
  DecodeModRm(PInst);
  if PInst^.ModRm.Value.IsMem then
  begin
    case PInst^.ModRm.Value.Reg of
      $00: Decode_INVALID(PInst);
      $01:
        begin
          if PInst^.Fields.W then
            Decode_GP_CMPXCHG16B_Mo(PInst)
          else
            Decode_GP_CMPXCHG8B_Mq(PInst);
        end;
      $02: Decode_INSX_XRSTORS_M(PInst);
      $03: Decode_INSX_XSAVEC_M(PInst);
      $04: Decode_INSX_XSAVES_M(PInst);
      $05: Decode_INVALID(PInst);
      $06:
        begin
          if PInst^.InternalData.MandatoryPrefixes and MND_PRF_66 <> 0 then
            Decode_VME_VMCLEAR_Mq(PInst)
          else if PInst^.InternalData.MandatoryPrefixes and MND_PRF_F3 <> 0 then
            Decode_VME_VMXON_Mq(PInst)
          else if PInst^.InternalData.MandatoryPrefixes and MND_PRF_NA = MND_PRF_NA then
            Decode_VME_VMPTRLD_Mq(PInst);
        end;
      $07: Decode_VME_VMPTRST_Mq(PInst);
    end;
  end
  else
  begin
    case PInst^.ModRm.Value.Reg of
      $06: Decode_GP_RDRAND_Rv(PInst);
      $07: Decode_GP_RDSEED_Rv(PInst);
    else Decode_INVALID(PInst);
    end;
  end;
end;

procedure Decode_GROUP_XOP1(PInst: PInstruction);
const
  GRP_XOP1_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_TBM_BLCFILL_By_Ey, { }
    Decode_TBM_BLSFILL_By_Ey, { }
    Decode_TBM_BLCS_By_Ey, { }
    Decode_TBM_TZMSK_By_Ey, { }
    Decode_TBM_BLCIC_By_Ey, { }
    Decode_TBM_BLSIC_By_Ey, { }
    Decode_TBM_T1MSKC_By_Ey { }
    );
begin
  PInst^.SetGroup(GF_GROUP_XOP1);
  DecodeModRm(PInst);
  GRP_XOP1_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_GROUP_XOP2(PInst: PInstruction);
const
  GRP_XOP2_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_INVALID, { }
    Decode_TBM_BLCMSK_By_Ey, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_TBM_BLCI_By_Ey, { }
    Decode_INVALID { } );
begin
  PInst^.SetGroup(GF_GROUP_XOP2);
  DecodeModRm(PInst);
  GRP_XOP2_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_GROUP_XOP3(PInst: PInstruction);
const
  GRP_XOP3_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_LWP_LLWPCB_Ry, { }
    Decode_LWP_SLWPCB_Ry, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID { } );
begin
  PInst^.SetGroup(GF_GROUP_XOP3);
  DecodeModRm(PInst);
  GRP_XOP3_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_GROUP_XOP4(PInst: PInstruction);
const
  GRP_XOP4_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_LWP_LWPINS_By_Ed_Id, { }
    Decode_LWP_LWPVAL_By_Ed_Id, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID, { }
    Decode_INVALID { } );
begin
  PInst^.SetGroup(GF_GROUP_XOP4);
  DecodeModRm(PInst);
  GRP_XOP4_Array[PInst^.ModRm.Value.Reg](PInst);
end;
