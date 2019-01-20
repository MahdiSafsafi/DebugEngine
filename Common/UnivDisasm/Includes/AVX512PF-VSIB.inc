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
// The Original Code is AVX512PF-VSIB.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512PF_VSIB_VGATHERPF0DPS_Md_z_K_not_0(PInst: PInstruction);
begin
  // VGATHERPF0DPS  Md.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERPF0DPS;
  Decode_vsib_Md_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERPF0DPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VGATHERPF0DPD_Mq_y_K_not_0(PInst: PInstruction);
begin
  // VGATHERPF0DPD  Mq.y{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERPF0DPD;
  Decode_vsib_Mq_y_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERPF0DPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VGATHERPF1DPS_Md_z_K_not_0(PInst: PInstruction);
begin
  // VGATHERPF1DPS  Md.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERPF1DPS;
  Decode_vsib_Md_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERPF1DPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VGATHERPF1DPD_Mq_y_K_not_0(PInst: PInstruction);
begin
  // VGATHERPF1DPD  Mq.y{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERPF1DPD;
  Decode_vsib_Mq_y_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERPF1DPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VSCATTERPF0DPS_Md_z_K_not_0(PInst: PInstruction);
begin
  // VSCATTERPF0DPS  Md.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VSCATTERPF0DPS;
  Decode_vsib_Md_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERPF0DPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VSCATTERPF0DPD_Mq_y_K_not_0(PInst: PInstruction);
begin
  // VSCATTERPF0DPD  Mq.y{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VSCATTERPF0DPD;
  Decode_vsib_Mq_y_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERPF0DPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VSCATTERPF1DPS_Md_z_K_not_0(PInst: PInstruction);
begin
  // VSCATTERPF1DPS  Md.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VSCATTERPF1DPS;
  Decode_vsib_Md_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERPF1DPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VSCATTERPF1DPD_Mq_y_K_not_0(PInst: PInstruction);
begin
  // VSCATTERPF1DPD  Mq.y{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VSCATTERPF1DPD;
  Decode_vsib_Mq_y_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERPF1DPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VGATHERPF0QPS_Md_z_K_not_0(PInst: PInstruction);
begin
  // VGATHERPF0QPS  Md.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERPF0QPS;
  Decode_vsib_Md_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERPF0QPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VGATHERPF0QPD_Mq_z_K_not_0(PInst: PInstruction);
begin
  // VGATHERPF0QPD  Mq.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERPF0QPD;
  Decode_vsib_Mq_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERPF0QPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VGATHERPF1QPS_Md_z_K_not_0(PInst: PInstruction);
begin
  // VGATHERPF1QPS  Md.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERPF1QPS;
  Decode_vsib_Md_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERPF1QPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VGATHERPF1QPD_Mq_z_K_not_0(PInst: PInstruction);
begin
  // VGATHERPF1QPD  Mq.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VGATHERPF1QPD;
  Decode_vsib_Mq_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERPF1QPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VSCATTERPF0QPS_Md_z_K_not_0(PInst: PInstruction);
begin
  // VSCATTERPF0QPS  Md.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VSCATTERPF0QPS;
  Decode_vsib_Md_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERPF0QPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VSCATTERPF0QPD_Mq_z_K_not_0(PInst: PInstruction);
begin
  // VSCATTERPF0QPD  Mq.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VSCATTERPF0QPD;
  Decode_vsib_Mq_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERPF0QPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VSCATTERPF1QPS_Md_z_K_not_0(PInst: PInstruction);
begin
  // VSCATTERPF1QPS  Md.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VSCATTERPF1QPS;
  Decode_vsib_Md_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERPF1QPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512PF_VSIB_VSCATTERPF1QPD_Mq_z_K_not_0(PInst: PInstruction);
begin
  // VSCATTERPF1QPD  Mq.z{K.!0}
  PInst^.InstGroups := (INST_GRP_AVX512PF or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T1S);
  PInst^.InstID := INST_ID_VSCATTERPF1QPD;
  Decode_vsib_Mq_z_K_not_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSCATTERPF1QPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
