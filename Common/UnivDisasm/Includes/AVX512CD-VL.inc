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
// The Original Code is AVX512CD-VL.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512CD_VL_VPBROADCASTMB2Q_Vn_mK(PInst: PInstruction);
begin
  // VPBROADCASTMB2Q  Vn,mK
  PInst^.InstGroups := (INST_GRP_AVX512CD or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTMB2Q;
  Decode_Vn_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTMB2Q);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512CD_VL_VPBROADCASTMW2D_Vn_mK(PInst: PInstruction);
begin
  // VPBROADCASTMW2D  Vn,mK
  PInst^.InstGroups := (INST_GRP_AVX512CD or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTMW2D;
  Decode_Vn_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTMW2D);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512CD_VL_VPLZCNTQ_Vn_K_z_B64_Wn(PInst: PInstruction);
begin
  // VPLZCNTQ  Vn{K}{z},B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512CD or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPLZCNTQ;
  Decode_Vn_K_z_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPLZCNTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512CD_VL_VPLZCNTD_Vn_K_z_B32_Wn(PInst: PInstruction);
begin
  // VPLZCNTD  Vn{K}{z},B32(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512CD or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPLZCNTD;
  Decode_Vn_K_z_B32_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPLZCNTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512CD_VL_VPCONFLICTQ_Vn_K_z_B64_Wn(PInst: PInstruction);
begin
  // VPCONFLICTQ  Vn{K}{z},B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512CD or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPCONFLICTQ;
  Decode_Vn_K_z_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCONFLICTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512CD_VL_VPCONFLICTD_Vn_K_z_B32_Wn(PInst: PInstruction);
begin
  // VPCONFLICTD  Vn{K}{z},B32(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512CD or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPCONFLICTD;
  Decode_Vn_K_z_B32_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCONFLICTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
