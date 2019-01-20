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
// The Original Code is AVX512F-VL-VSIB.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512F_VL_VSIB_VPGATHERDQ_Vn_K_not_0_Mq_h
  (PInst: PInstruction);
begin
  // VPGATHERDQ  Vn{K.!0},Mq.h
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.InstID := INST_ID_VPGATHERDQ;
  Decode_vsib_Vn_K_not_0_Mq_h(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPGATHERDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VPGATHERDD_Vn_K_not_0_Md_n
  (PInst: PInstruction);
begin
  // VPGATHERDD  Vn{K.!0},Md.n
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.InstID := INST_ID_VPGATHERDD;
  Decode_vsib_Vn_K_not_0_Md_n(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPGATHERDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VPGATHERQQ_Vn_K_not_0_Mq_n
  (PInst: PInstruction);
begin
  // VPGATHERQQ  Vn{K.!0},Mq.n
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.InstID := INST_ID_VPGATHERQQ;
  Decode_vsib_Vn_K_not_0_Mq_n(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPGATHERQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VPGATHERQD_Vh_K_not_0_Md_n
  (PInst: PInstruction);
begin
  // VPGATHERQD  Vh{K.!0},Md.n
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.InstID := INST_ID_VPGATHERQD;
  Decode_vsib_Vh_K_not_0_Md_n(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPGATHERQD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VGATHERDPD_Vn_K_not_0_Mq_h
  (PInst: PInstruction);
begin
  // VGATHERDPD  Vn{K.!0},Mq.h
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERDPD;
  Decode_vsib_Vn_K_not_0_Mq_h(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VGATHERDPS_Vn_K_not_0_Md_n
  (PInst: PInstruction);
begin
  // VGATHERDPS  Vn{K.!0},Md.n
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERDPS;
  Decode_vsib_Vn_K_not_0_Md_n(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VGATHERQPD_Vn_K_not_0_Mq_n
  (PInst: PInstruction);
begin
  // VGATHERQPD  Vn{K.!0},Mq.n
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VGATHERQPD;
  Decode_vsib_Vn_K_not_0_Mq_n(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERQPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VGATHERQPS_Vh_K_not_0_Md_n
  (PInst: PInstruction);
begin
  // VGATHERQPS  Vh{K.!0},Md.n
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VGATHERQPS;
  Decode_vsib_Vh_K_not_0_Md_n(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERQPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VPSCATTERDQ_Mq_h_K_not_0_Vn
  (PInst: PInstruction);
begin
  // VPSCATTERDQ  Mq.h{K.!0},Vn
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VPSCATTERDQ;
  Decode_vsib_Mq_h_K_not_0_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSCATTERDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VPSCATTERDD_Md_n_K_not_0_Vn
  (PInst: PInstruction);
begin
  // VPSCATTERDD  Md.n{K.!0},Vn
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_32);
  PInst^.InstID := INST_ID_VPSCATTERDD;
  Decode_vsib_Md_n_K_not_0_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSCATTERDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VPSCATTERQQ_Mq_n_K_not_0_Vn
  (PInst: PInstruction);
begin
  // VPSCATTERQQ  Mq.n{K.!0},Vn
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VPSCATTERQQ;
  Decode_vsib_Mq_n_K_not_0_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSCATTERQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VPSCATTERQD_Md_n_K_not_0_Vh
  (PInst: PInstruction);
begin
  // VPSCATTERQD  Md.n{K.!0},Vh
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VPSCATTERQD;
  Decode_vsib_Md_n_K_not_0_Vh(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSCATTERQD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VSCATTERDPD_Mq_h_K_not_0_Vn
  (PInst: PInstruction);
begin
  // VSCATTERDPD  Mq.h{K.!0},Vn
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VSCATTERDPD;
  Decode_vsib_Mq_h_K_not_0_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VSCATTERDPS_Md_n_K_not_0_Vn
  (PInst: PInstruction);
begin
  // VSCATTERDPS  Md.n{K.!0},Vn
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_32);
  PInst^.InstID := INST_ID_VSCATTERDPS;
  Decode_vsib_Md_n_K_not_0_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VSCATTERQPD_Mq_n_K_not_0_Vn
  (PInst: PInstruction);
begin
  // VSCATTERQPD  Mq.n{K.!0},Vn
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VSCATTERQPD;
  Decode_vsib_Mq_n_K_not_0_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERQPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512F_VL_VSIB_VSCATTERQPS_Md_n_K_not_0_Vh
  (PInst: PInstruction);
begin
  // VSCATTERQPS  Md.n{K.!0},Vh
  PInst^.InstGroups := (INST_GRP_AVX512F or INST_GRP_VL or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VSCATTERQPS;
  Decode_vsib_Md_n_K_not_0_Vh(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERQPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
