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
// The Original Code is AVX512F.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512F_VBROADCASTF64X4_Vz_K_z_My(PInst: PInstruction);
begin
  // VBROADCASTF64X4  Vz{K}{z},My
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T4);
  PInst^.InstID := INST_ID_VBROADCASTF64X4;
  Decode_Vz_K_z_My(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTF64X4);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VSCALEFSD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VSCALEFSD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VSCALEFSD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCALEFSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VSCALEFSS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VSCALEFSS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VSCALEFSS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCALEFSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VGETEXPSD_Vo_K_z_Ho_Wo_q_sae(PInst: PInstruction);
begin
  // VGETEXPSD  Vo{K}{z},Ho,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VGETEXPSD;
  Decode_Vo_K_z_Ho_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGETEXPSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VGETEXPSS_Vo_K_z_Ho_Wo_d_sae(PInst: PInstruction);
begin
  // VGETEXPSS  Vo{K}{z},Ho,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VGETEXPSS;
  Decode_Vo_K_z_Ho_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGETEXPSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VRCP14SD_Vo_K_z_Ho_Wo_q(PInst: PInstruction);
begin
  // VRCP14SD  Vo{K}{z},Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRCP14SD;
  Decode_Vo_K_z_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCP14SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VRCP14SS_Vo_K_z_Ho_Wo_d(PInst: PInstruction);
begin
  // VRCP14SS  Vo{K}{z},Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRCP14SS;
  Decode_Vo_K_z_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRCP14SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VRSQRT14SD_Vo_K_z_Ho_Wo_q(PInst: PInstruction);
begin
  // VRSQRT14SD  Vo{K}{z},Ho,Wo.q
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRSQRT14SD;
  Decode_Vo_K_z_Ho_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRT14SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VRSQRT14SS_Vo_K_z_Ho_Wo_d(PInst: PInstruction);
begin
  // VRSQRT14SS  Vo{K}{z},Ho,Wo.d
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRSQRT14SS;
  Decode_Vo_K_z_Ho_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRSQRT14SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VBROADCASTI64X4_Vz_K_z_My(PInst: PInstruction);
begin
  // VBROADCASTI64X4  Vz{K}{z},My
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T4);
  PInst^.InstID := INST_ID_VBROADCASTI64X4;
  Decode_Vz_K_z_My(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTI64X4);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMADD132SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFMADD132SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMADD132SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD132SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMADD132SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFMADD132SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMADD132SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD132SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMSUB132SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFMSUB132SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMSUB132SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB132SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMSUB132SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFMSUB132SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMSUB132SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB132SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMADD132SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFNMADD132SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMADD132SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD132SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMADD132SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFNMADD132SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMADD132SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD132SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMSUB132SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFNMSUB132SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMSUB132SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB132SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMSUB132SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFNMSUB132SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMSUB132SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB132SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMADD213SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFMADD213SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMADD213SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD213SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMADD213SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFMADD213SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMADD213SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD213SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMSUB213SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFMSUB213SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMSUB213SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB213SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMSUB213SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFMSUB213SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMSUB213SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB213SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMADD213SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFNMADD213SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMADD213SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD213SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMADD213SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFNMADD213SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMADD213SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD213SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMSUB213SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFNMSUB213SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMSUB213SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB213SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMSUB213SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFNMSUB213SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMSUB213SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB213SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMADD231SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFMADD231SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMADD231SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD231SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMADD231SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFMADD231SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMADD231SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD231SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMSUB231SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFMSUB231SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMSUB231SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB231SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFMSUB231SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFMSUB231SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFMSUB231SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB231SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMADD231SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFNMADD231SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMADD231SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD231SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMADD231SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFNMADD231SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMADD231SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD231SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMSUB231SD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VFNMSUB231SD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMSUB231SD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB231SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFNMSUB231SS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VFNMSUB231SS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFNMSUB231SS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB231SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VRNDSCALESS_Vo_K_z_Ho_Wo_d_sae_Ib(PInst: PInstruction);
begin
  // VRNDSCALESS  Vo{K}{z},Ho,Wo.d{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRNDSCALESS;
  Decode_Vo_K_z_Ho_Wo_d_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRNDSCALESS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VRNDSCALESD_Vo_K_z_Ho_Wo_q_sae_Ib(PInst: PInstruction);
begin
  // VRNDSCALESD  Vo{K}{z},Ho,Wo.q{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRNDSCALESD;
  Decode_Vo_K_z_Ho_Wo_q_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRNDSCALESD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VEXTRACTPS_Md_Vo_Ib(PInst: PInstruction);
begin
  // VEXTRACTPS  Md,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VEXTRACTPS;
  Decode_Md_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VEXTRACTPS_Ry_Vo_Ib(PInst: PInstruction);
begin
  // VEXTRACTPS  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VEXTRACTPS;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VINSERTF64X4_Vz_K_z_Hz_Wy_Ib(PInst: PInstruction);
begin
  // VINSERTF64X4  Vz{K}{z},Hz,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T4);
  PInst^.InstID := INST_ID_VINSERTF64X4;
  Decode_Vz_K_z_Hz_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTF64X4);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VEXTRACTF64X4_Wy_K_z_Vz_Ib(PInst: PInstruction);
begin
  // VEXTRACTF64X4  Wy{K}{z},Vz,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T4);
  PInst^.InstID := INST_ID_VEXTRACTF64X4;
  Decode_Wy_K_z_Vz_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTF64X4);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VINSERTPS_Vo_Ho_Md_Ib(PInst: PInstruction);
begin
  // VINSERTPS  Vo,Ho,Md,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VINSERTPS;
  Decode_Vo_Ho_Md_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VINSERTPS_Vo_Ho_Uo_Ib(PInst: PInstruction);
begin
  // VINSERTPS  Vo,Ho,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VINSERTPS;
  Decode_Vo_Ho_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VGETMANTSD_Vo_K_z_Ho_Wo_q_sae_Ib(PInst: PInstruction);
begin
  // VGETMANTSD  Vo{K}{z},Ho,Wo.q{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VGETMANTSD;
  Decode_Vo_K_z_Ho_Wo_q_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGETMANTSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VGETMANTSS_Vo_K_z_Ho_Wo_d_sae_Ib(PInst: PInstruction);
begin
  // VGETMANTSS  Vo{K}{z},Ho,Wo.d{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VGETMANTSS;
  Decode_Vo_K_z_Ho_Wo_d_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGETMANTSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KSHIFTRW_rK_mK_Ib(PInst: PInstruction);
begin
  // KSHIFTRW  rK,mK,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KSHIFTRW;
  Decode_rK_mK_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KSHIFTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KSHIFTLW_rK_mK_Ib(PInst: PInstruction);
begin
  // KSHIFTLW  rK,mK,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KSHIFTLW;
  Decode_rK_mK_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KSHIFTLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VINSERTI64X4_Vz_K_z_Hz_Wy_Ib(PInst: PInstruction);
begin
  // VINSERTI64X4  Vz{K}{z},Hz,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T4);
  PInst^.InstID := INST_ID_VINSERTI64X4;
  Decode_Vz_K_z_Hz_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTI64X4);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VEXTRACTI64X4_Wy_K_z_Vz_Ib(PInst: PInstruction);
begin
  // VEXTRACTI64X4  Wy{K}{z},Vz,Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T4);
  PInst^.InstID := INST_ID_VEXTRACTI64X4;
  Decode_Wy_K_z_Vz_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTI64X4);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFIXUPIMMSD_Vo_K_z_Ho_Wo_q_sae_Ib(PInst: PInstruction);
begin
  // VFIXUPIMMSD  Vo{K}{z},Ho,Wo.q{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFIXUPIMMSD;
  Decode_Vo_K_z_Ho_Wo_q_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFIXUPIMMSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VFIXUPIMMSS_Vo_K_z_Ho_Wo_d_sae_Ib(PInst: PInstruction);
begin
  // VFIXUPIMMSS  Vo{K}{z},Ho,Wo.d{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFIXUPIMMSS;
  Decode_Vo_K_z_Ho_Wo_d_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFIXUPIMMSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVSD_Vo_K_z_Mo_q(PInst: PInstruction);
begin
  // VMOVSD  Vo{K}{z},Mo.q
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVSD;
  Decode_Vo_K_z_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVSD_Vo_K_z_Ho_Uo(PInst: PInstruction);
begin
  // VMOVSD  Vo{K}{z},Ho,Uo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVSD;
  Decode_Vo_K_z_Ho_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVSS_Vo_K_z_Mo_d(PInst: PInstruction);
begin
  // VMOVSS  Vo{K}{z},Mo.d
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVSS;
  Decode_Vo_K_z_Mo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVSS_Vo_K_z_Ho_Uo(PInst: PInstruction);
begin
  // VMOVSS  Vo{K}{z},Ho,Uo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVSS;
  Decode_Vo_K_z_Ho_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVSD_Mo_q_K_Vo(PInst: PInstruction);
begin
  // VMOVSD  Mo.q{K},Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVSD;
  Decode_Mo_q_K_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVSD_Uo_K_z_Ho_Vo(PInst: PInstruction);
begin
  // VMOVSD  Uo{K}{z},Ho,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVSD;
  Decode_Uo_K_z_Ho_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVSS_Mo_d_K_Vo(PInst: PInstruction);
begin
  // VMOVSS  Mo.d{K},Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVSS;
  Decode_Mo_d_K_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVSS_Uo_K_z_Ho_Vo(PInst: PInstruction);
begin
  // VMOVSS  Uo{K}{z},Ho,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVSS;
  Decode_Uo_K_z_Ho_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVLPS_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  // VMOVLPS  Vo,Ho,Mo.q
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL128(TT_T2);
  PInst^.InstID := INST_ID_VMOVLPS;
  Decode_Vo_Ho_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVHLPS_Vo_Ho_Uo(PInst: PInstruction);
begin
  // VMOVHLPS  Vo,Ho,Uo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL128(TT_T2);
  PInst^.InstID := INST_ID_VMOVHLPS;
  Decode_Vo_Ho_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVLPD_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  // VMOVLPD  Vo,Ho,Mo.q
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVLPD;
  Decode_Vo_Ho_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVLPD_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVLPD  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVLPD;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVLPS_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVLPS  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL128(TT_T2);
  PInst^.InstID := INST_ID_VMOVLPS;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVHPS_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  // VMOVHPS  Vo,Ho,Mo.q
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL128(TT_T2);
  PInst^.InstID := INST_ID_VMOVHPS;
  Decode_Vo_Ho_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVLHPS_Vo_Ho_Uo(PInst: PInstruction);
begin
  // VMOVLHPS  Vo,Ho,Uo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL128(TT_T2);
  PInst^.InstID := INST_ID_VMOVLHPS;
  Decode_Vo_Ho_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVLHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVHPD_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  // VMOVHPD  Vo,Ho,Mo.q
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVHPD;
  Decode_Vo_Ho_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVHPD_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVHPD  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVHPD;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVHPS_Mo_q_Vo(PInst: PInstruction);
begin
  // VMOVHPS  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL128(TT_T2);
  PInst^.InstID := INST_ID_VMOVHPS;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSI2SD_Vo_Ho_Eq_er(PInst: PInstruction);
begin
  // VCVTSI2SD  Vo,Ho,Eq{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTSI2SD;
  Decode_Vo_Ho_Eq_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSI2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSI2SS_Vo_Ho_Eq_er(PInst: PInstruction);
begin
  // VCVTSI2SS  Vo,Ho,Eq{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTSI2SS;
  Decode_Vo_Ho_Eq_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSI2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSI2SD_Vo_Ho_Ed_er(PInst: PInstruction);
begin
  // VCVTSI2SD  Vo,Ho,Ed{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTSI2SD;
  Decode_Vo_Ho_Ed_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSI2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSI2SS_Vo_Ho_Ed_er(PInst: PInstruction);
begin
  // VCVTSI2SS  Vo,Ho,Ed{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTSI2SS;
  Decode_Vo_Ho_Ed_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSI2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTTSD2SI_Gq_Wo_q_sae(PInst: PInstruction);
begin
  // VCVTTSD2SI  Gq,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTTSD2SI;
  Decode_Gq_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSD2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTTSS2SI_Gq_Wo_d_sae(PInst: PInstruction);
begin
  // VCVTTSS2SI  Gq,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTTSS2SI;
  Decode_Gq_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSS2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTTSD2SI_Gd_Wo_q_sae(PInst: PInstruction);
begin
  // VCVTTSD2SI  Gd,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTTSD2SI;
  Decode_Gd_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSD2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTTSS2SI_Gd_Wo_d_sae(PInst: PInstruction);
begin
  // VCVTTSS2SI  Gd,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTTSS2SI;
  Decode_Gd_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSS2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSD2SI_Gq_Wo_q_er(PInst: PInstruction);
begin
  // VCVTSD2SI  Gq,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTSD2SI;
  Decode_Gq_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSD2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSS2SI_Gq_Wo_d_er(PInst: PInstruction);
begin
  // VCVTSS2SI  Gq,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTSS2SI;
  Decode_Gq_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSS2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSD2SI_Gd_Wo_q_er(PInst: PInstruction);
begin
  // VCVTSD2SI  Gd,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTSD2SI;
  Decode_Gd_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSD2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSS2SI_Gd_Wo_d_er(PInst: PInstruction);
begin
  // VCVTSS2SI  Gd,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTSS2SI;
  Decode_Gd_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSS2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VUCOMISD_Vo_Wo_q_sae(PInst: PInstruction);
begin
  // VUCOMISD  Vo,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VUCOMISD;
  Decode_Vo_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUCOMISD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VUCOMISS_Vo_Wo_d_sae(PInst: PInstruction);
begin
  // VUCOMISS  Vo,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VUCOMISS;
  Decode_Vo_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VUCOMISS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCOMISD_Vo_Wo_q_sae(PInst: PInstruction);
begin
  // VCOMISD  Vo,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCOMISD;
  Decode_Vo_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCOMISD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCOMISS_Vo_Wo_d_sae(PInst: PInstruction);
begin
  // VCOMISS  Vo,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCOMISS;
  Decode_Vo_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCOMISS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KANDW_rK_vK_mK(PInst: PInstruction);
begin
  // KANDW  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KANDW;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KANDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KANDNW_rK_vK_mK(PInst: PInstruction);
begin
  // KANDNW  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KANDNW;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KANDNW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KNOTW_rK_mK(PInst: PInstruction);
begin
  // KNOTW  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KNOTW;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KNOTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KORW_rK_vK_mK(PInst: PInstruction);
begin
  // KORW  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KORW;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KORW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KXNORW_rK_vK_mK(PInst: PInstruction);
begin
  // KXNORW  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KXNORW;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KXNORW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KXORW_rK_vK_mK(PInst: PInstruction);
begin
  // KXORW  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KXORW;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KXORW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KUNPCKBW_rK_vK_mK(PInst: PInstruction);
begin
  // KUNPCKBW  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KUNPCKBW;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KUNPCKBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VSQRTSD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VSQRTSD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VSQRTSD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSQRTSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VSQRTSS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VSQRTSS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VSQRTSS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSQRTSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VADDSD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VADDSD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VADDSD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VADDSS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VADDSS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VADDSS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMULSD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VMULSD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMULSD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMULSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMULSS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VMULSS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMULSS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMULSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSD2SS_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VCVTSD2SS  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTSD2SS;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSD2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSS2SD_Vo_K_z_Ho_Wo_d_sae(PInst: PInstruction);
begin
  // VCVTSS2SD  Vo{K}{z},Ho,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTSS2SD;
  Decode_Vo_K_z_Ho_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSS2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VSUBSD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VSUBSD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VSUBSD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VSUBSS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VSUBSS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VSUBSS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMINSD_Vo_K_z_Ho_Wo_q_sae(PInst: PInstruction);
begin
  // VMINSD  Vo{K}{z},Ho,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMINSD;
  Decode_Vo_K_z_Ho_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMINSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMINSS_Vo_K_z_Ho_Wo_d_sae(PInst: PInstruction);
begin
  // VMINSS  Vo{K}{z},Ho,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMINSS;
  Decode_Vo_K_z_Ho_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMINSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VDIVSD_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  // VDIVSD  Vo{K}{z},Ho,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VDIVSD;
  Decode_Vo_K_z_Ho_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDIVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VDIVSS_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  // VDIVSS  Vo{K}{z},Ho,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VDIVSS;
  Decode_Vo_K_z_Ho_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDIVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMAXSD_Vo_K_z_Ho_Wo_q_sae(PInst: PInstruction);
begin
  // VMAXSD  Vo{K}{z},Ho,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMAXSD;
  Decode_Vo_K_z_Ho_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMAXSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMAXSS_Vo_K_z_Ho_Wo_d_sae(PInst: PInstruction);
begin
  // VMAXSS  Vo{K}{z},Ho,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMAXSS;
  Decode_Vo_K_z_Ho_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMAXSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVQ_Vo_Eq(PInst: PInstruction);
begin
  // VMOVQ  Vo,Eq
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVQ;
  Decode_Vo_Eq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVD_Vo_Ed(PInst: PInstruction);
begin
  // VMOVD  Vo,Ed
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVD;
  Decode_Vo_Ed(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTTSD2USI_Gq_Wo_q_sae(PInst: PInstruction);
begin
  // VCVTTSD2USI  Gq,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTTSD2USI;
  Decode_Gq_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSD2USI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTTSS2USI_Gq_Wo_d_sae(PInst: PInstruction);
begin
  // VCVTTSS2USI  Gq,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTTSS2USI;
  Decode_Gq_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSS2USI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTTSD2USI_Gd_Wo_q_sae(PInst: PInstruction);
begin
  // VCVTTSD2USI  Gd,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTTSD2USI;
  Decode_Gd_Wo_q_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSD2USI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTTSS2USI_Gd_Wo_d_sae(PInst: PInstruction);
begin
  // VCVTTSS2USI  Gd,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTTSS2USI;
  Decode_Gd_Wo_d_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTSS2USI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSD2USI_Gq_Wo_q_er(PInst: PInstruction);
begin
  // VCVTSD2USI  Gq,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTSD2USI;
  Decode_Gq_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSD2USI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSS2USI_Gq_Wo_d_er(PInst: PInstruction);
begin
  // VCVTSS2USI  Gq,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTSS2USI;
  Decode_Gq_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSS2USI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSD2USI_Gd_Wo_q_er(PInst: PInstruction);
begin
  // VCVTSD2USI  Gd,Wo.q{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTSD2USI;
  Decode_Gd_Wo_q_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSD2USI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTSS2USI_Gd_Wo_d_er(PInst: PInstruction);
begin
  // VCVTSS2USI  Gd,Wo.d{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1F);
  PInst^.InstID := INST_ID_VCVTSS2USI;
  Decode_Gd_Wo_d_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTSS2USI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTUSI2SD_Vo_Ho_Eq_er(PInst: PInstruction);
begin
  // VCVTUSI2SD  Vo,Ho,Eq{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTUSI2SD;
  Decode_Vo_Ho_Eq_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTUSI2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTUSI2SS_Vo_Ho_Eq_er(PInst: PInstruction);
begin
  // VCVTUSI2SS  Vo,Ho,Eq{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTUSI2SS;
  Decode_Vo_Ho_Eq_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTUSI2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTUSI2SD_Vo_Ho_Ed_er(PInst: PInstruction);
begin
  // VCVTUSI2SD  Vo,Ho,Ed{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTUSI2SD;
  Decode_Vo_Ho_Ed_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTUSI2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCVTUSI2SS_Vo_Ho_Ed_er(PInst: PInstruction);
begin
  // VCVTUSI2SS  Vo,Ho,Ed{er}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VCVTUSI2SS;
  Decode_Vo_Ho_Ed_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTUSI2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVQ_Eq_Vo(PInst: PInstruction);
begin
  // VMOVQ  Eq,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVQ;
  Decode_Eq_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVQ_Vo_Wo_q(PInst: PInstruction);
begin
  // VMOVQ  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVQ;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVD_Ed_Vo(PInst: PInstruction);
begin
  // VMOVD  Ed,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVD;
  Decode_Ed_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KMOVW_rK_Mw(PInst: PInstruction);
begin
  // KMOVW  rK,Mw
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KMOVW;
  Decode_rK_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KMOVW_rK_mK(PInst: PInstruction);
begin
  // KMOVW  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KMOVW;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KMOVW_Mw_rK(PInst: PInstruction);
begin
  // KMOVW  Mw,rK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KMOVW;
  Decode_Mw_rK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KMOVW_rK_Ry(PInst: PInstruction);
begin
  // KMOVW  rK,Ry
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KMOVW;
  Decode_rK_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KMOVW_Gy_mK(PInst: PInstruction);
begin
  // KMOVW  Gy,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KMOVW;
  Decode_Gy_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_KORTESTW_rK_mK(PInst: PInstruction);
begin
  // KORTESTW  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KORTESTW;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KORTESTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCMPccSD_rK_K_Ho_Wo_q_sae(PInst: PInstruction);
begin
  // VCMPccSD  rK{K},Ho,Wo.q{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  Decode_rK_K_Ho_Wo_q_sae(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VCMPccSD_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VCMPccSD_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VCMPccSS_rK_K_Ho_Wo_d_sae(PInst: PInstruction);
begin
  // VCMPccSS  rK{K},Ho,Wo.d{sae}
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  Decode_rK_K_Ho_Wo_d_sae(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VCMPccSS_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VCMPccSS_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VMOVQ_Wo_q_Vo(PInst: PInstruction);
begin
  // VMOVQ  Wo.q,Vo
  PInst^.InstGroups := INST_GRP_AVX512F;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VMOVQ;
  Decode_Wo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
