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
// The Original Code is AVX512ER.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512ER_VEXP2PD_Vz_K_z_B64_Wz_sae(PInst: PInstruction);
begin
  // VEXP2PD  Vz{K}{z},B64(Wz){sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_FV);
  PInst^.InstID := INST_ID_VEXP2PD;
  Decode_Vz_K_z_B64_Wz_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXP2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VEXP2PS_Vz_K_z_B32_Wz_sae(PInst: PInstruction);
begin
  // VEXP2PS  Vz{K}{z},B32(Wz){sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_FV);
  PInst^.InstID := INST_ID_VEXP2PS;
  Decode_Vz_K_z_B32_Wz_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXP2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VRCP28PD_Vz_K_z_B64_Wz_sae(PInst: PInstruction);
begin
  // VRCP28PD  Vz{K}{z},B64(Wz){sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_FV);
  PInst^.InstID := INST_ID_VRCP28PD;
  Decode_Vz_K_z_B64_Wz_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCP28PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VRCP28PS_Vz_K_z_B32_Wz_sae(PInst: PInstruction);
begin
  // VRCP28PS  Vz{K}{z},B32(Wz){sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_FV);
  PInst^.InstID := INST_ID_VRCP28PS;
  Decode_Vz_K_z_B32_Wz_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCP28PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VRCP28SD_Vo_K_z_Ho_Wo_q_sae(PInst: PInstruction);
begin
  // VRCP28SD  Vo{K}{z},Ho,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRCP28SD;
  Decode_Vo_K_z_Ho_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCP28SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VRCP28SS_Vo_K_z_Ho_Wo_d_sae(PInst: PInstruction);
begin
  // VRCP28SS  Vo{K}{z},Ho,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRCP28SS;
  Decode_Vo_K_z_Ho_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCP28SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VRSQRT28PD_Vz_K_z_B64_Wz_sae(PInst: PInstruction);
begin
  // VRSQRT28PD  Vz{K}{z},B64(Wz){sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_FV);
  PInst^.InstID := INST_ID_VRSQRT28PD;
  Decode_Vz_K_z_B64_Wz_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRT28PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VRSQRT28PS_Vz_K_z_B32_Wz_sae(PInst: PInstruction);
begin
  // VRSQRT28PS  Vz{K}{z},B32(Wz){sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_FV);
  PInst^.InstID := INST_ID_VRSQRT28PS;
  Decode_Vz_K_z_B32_Wz_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRT28PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VRSQRT28SD_Vo_K_z_Ho_Wo_q_sae(PInst: PInstruction);
begin
  // VRSQRT28SD  Vo{K}{z},Ho,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRSQRT28SD;
  Decode_Vo_K_z_Ho_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRT28SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512ER_VRSQRT28SS_Vo_K_z_Ho_Wo_d_sae(PInst: PInstruction);
begin
  // VRSQRT28SS  Vo{K}{z},Ho,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512ER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRSQRT28SS;
  Decode_Vo_K_z_Ho_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRT28SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
