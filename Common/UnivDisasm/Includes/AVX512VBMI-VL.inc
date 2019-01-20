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
// The Original Code is AVX512VBMI-VL.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512VBMI_VL_VPERMI2B_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPERMI2B  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512VBMI or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPERMI2B;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMI2B);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512VBMI_VL_VPERMT2B_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPERMT2B  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512VBMI or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPERMT2B;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMT2B);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512VBMI_VL_VPMULTISHIFTQB_Vn_K_z_Hn_B64_Wn
  (PInst: PInstruction);
begin
  // VPMULTISHIFTQB  Vn{K}{z},Hn,B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512VBMI or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPMULTISHIFTQB;
  Decode_Vn_K_z_Hn_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULTISHIFTQB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512VBMI_VL_VPERMB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPERMB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512VBMI or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPERMB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
