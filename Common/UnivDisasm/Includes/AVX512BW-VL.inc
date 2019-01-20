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
// The Original Code is AVX512BW-VL.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512BW_VL_VPSHUFB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSHUFB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSHUFB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMADDUBSW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMADDUBSW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMADDUBSW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMADDUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMULHRSW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMULHRSW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMULHRSW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHRSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSRLVW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSRLVW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSRLVW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLVW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVUSWB_Wh_K_z_Vn(PInst: PInstruction);
begin
  // VPMOVUSWB  Wh{K}{z},Vn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL(TT_HVM);
  PInst^.InstID := INST_ID_VPMOVUSWB;
  Decode_Wh_K_z_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVUSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSRAVW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSRAVW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSRAVW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAVW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSLLVW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSLLVW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSLLVW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLVW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPABSB_Vn_K_z_Wn(PInst: PInstruction);
begin
  // VPABSB  Vn{K}{z},Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPABSB;
  Decode_Vn_K_z_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPABSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPABSW_Vn_K_z_Wn(PInst: PInstruction);
begin
  // VPABSW  Vn{K}{z},Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPABSW;
  Decode_Vn_K_z_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPABSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVSWB_Wh_K_z_Vn(PInst: PInstruction);
begin
  // VPMOVSWB  Wh{K}{z},Vn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL(TT_HVM);
  PInst^.InstID := INST_ID_VPMOVSWB;
  Decode_Wh_K_z_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVSXBW_Vn_K_z_Wh(PInst: PInstruction);
begin
  // VPMOVSXBW  Vn{K}{z},Wh
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_HVM);
  PInst^.InstID := INST_ID_VPMOVSXBW;
  Decode_Vn_K_z_Wh(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVSXBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPTESTMW_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPTESTMW  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPTESTMW;
  Decode_rK_K_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPTESTMW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPTESTNMW_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPTESTNMW  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPTESTNMW;
  Decode_rK_K_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPTESTNMW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPTESTMB_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPTESTMB  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPTESTMB;
  Decode_rK_K_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPTESTMB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPTESTNMB_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPTESTNMB  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPTESTNMB;
  Decode_rK_K_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPTESTNMB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVM2W_Vn_mK(PInst: PInstruction);
begin
  // VPMOVM2W  Vn,mK
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPMOVM2W;
  Decode_Vn_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVM2W);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVM2B_Vn_mK(PInst: PInstruction);
begin
  // VPMOVM2B  Vn,mK
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPMOVM2B;
  Decode_Vn_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVM2B);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVW2M_rK_Un(PInst: PInstruction);
begin
  // VPMOVW2M  rK,Un
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPMOVW2M;
  Decode_rK_Un(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVW2M);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVB2M_rK_Un(PInst: PInstruction);
begin
  // VPMOVB2M  rK,Un
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPMOVB2M;
  Decode_rK_Un(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVB2M);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPACKUSDW_Vn_K_z_Hn_B32_Wn(PInst: PInstruction);
begin
  // VPACKUSDW  Vn{K}{z},Hn,B32(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPACKUSDW;
  Decode_Vn_K_z_Hn_B32_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKUSDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVWB_Wh_K_z_Vn(PInst: PInstruction);
begin
  // VPMOVWB  Wh{K}{z},Vn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL(TT_HVM);
  PInst^.InstID := INST_ID_VPMOVWB;
  Decode_Wh_K_z_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMOVZXBW_Vn_K_z_Wh(PInst: PInstruction);
begin
  // VPMOVZXBW  Vn{K}{z},Wh
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_HVM);
  PInst^.InstID := INST_ID_VPMOVZXBW;
  Decode_Vn_K_z_Wh(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVZXBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMINSB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMINSB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMINSB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMINUW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMINUW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMINUW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMAXSB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMAXSB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMAXSB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMAXUW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMAXUW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMAXUW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPBLENDMW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPBLENDMW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPBLENDMW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBLENDMW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPBLENDMB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPBLENDMB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPBLENDMB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBLENDMB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPERMI2W_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPERMI2W  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPERMI2W;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMI2W);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPBROADCASTB_Vn_K_z_Wo_b(PInst: PInstruction);
begin
  // VPBROADCASTB  Vn{K}{z},Wo.b
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.InstID := INST_ID_VPBROADCASTB;
  Decode_Vn_K_z_Wo_b(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPBROADCASTW_Vn_K_z_Wo_w(PInst: PInstruction);
begin
  // VPBROADCASTW  Vn{K}{z},Wo.w
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T1S);
  PInst^.InstID := INST_ID_VPBROADCASTW;
  Decode_Vn_K_z_Wo_w(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPBROADCASTB_Vn_K_z_Rb(PInst: PInstruction);
begin
  // VPBROADCASTB  Vn{K}{z},Rb
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTB;
  Decode_Vn_K_z_Rb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPBROADCASTW_Vn_K_z_Rw(PInst: PInstruction);
begin
  // VPBROADCASTW  Vn{K}{z},Rw
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPBROADCASTW;
  Decode_Vn_K_z_Rw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPBROADCASTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPERMT2W_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPERMT2W  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPERMT2W;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMT2W);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPERMW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPERMW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPERMW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPERMW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPALIGNR_Vn_K_z_Hn_Wn_Ib(PInst: PInstruction);
begin
  // VPALIGNR  Vn{K}{z},Hn,Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPALIGNR;
  Decode_Vn_K_z_Hn_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPALIGNR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPCMPccUW_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPCMPccUW  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  Decode_rK_K_Hn_Wn(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VPCMPccUW_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VPCMPccUW_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPCMPccUB_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPCMPccUB  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  Decode_rK_K_Hn_Wn(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VPCMPccUB_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VPCMPccUB_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPCMPccW_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPCMPccW  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  Decode_rK_K_Hn_Wn(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VPCMPccW_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VPCMPccW_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPCMPccB_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPCMPccB  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  Decode_rK_K_Hn_Wn(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_VPCMPccB_Array[PInst^.OpImmExt and $07];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_VPCMPccB_Array[PInst^.OpImmExt and $07]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VDBPSADBW_Vn_K_z_Hn_Wn_Ib(PInst: PInstruction);
begin
  // VDBPSADBW  Vn{K}{z},Hn,Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VDBPSADBW;
  Decode_Vn_K_z_Hn_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VDBPSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPUNPCKLBW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPUNPCKLBW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPUNPCKLBW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPUNPCKLWD_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPUNPCKLWD  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPUNPCKLWD;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKLWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPACKSSWB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPACKSSWB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPACKSSWB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKSSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPCMPGTB_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPCMPGTB  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPCMPGTB;
  Decode_rK_K_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPCMPGTW_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPCMPGTW  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPCMPGTW;
  Decode_rK_K_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPGTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPACKUSWB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPACKUSWB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPACKUSWB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKUSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPUNPCKHBW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPUNPCKHBW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPUNPCKHBW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPUNPCKHWD_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPUNPCKHWD  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPUNPCKHWD;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPUNPCKHWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPACKSSDW_Vn_K_z_Hn_B32_Wn(PInst: PInstruction);
begin
  // VPACKSSDW  Vn{K}{z},Hn,B32(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPACKSSDW;
  Decode_Vn_K_z_Hn_B32_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPACKSSDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VMOVDQU16_Vn_K_z_Wn(PInst: PInstruction);
begin
  // VMOVDQU16  Vn{K}{z},Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VMOVDQU16;
  Decode_Vn_K_z_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQU16);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VMOVDQU8_Vn_K_z_Wn(PInst: PInstruction);
begin
  // VMOVDQU8  Vn{K}{z},Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VMOVDQU8;
  Decode_Vn_K_z_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQU8);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSHUFLW_Vn_K_z_Wn_Ib(PInst: PInstruction);
begin
  // VPSHUFLW  Vn{K}{z},Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSHUFLW;
  Decode_Vn_K_z_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSHUFHW_Vn_K_z_Wn_Ib(PInst: PInstruction);
begin
  // VPSHUFHW  Vn{K}{z},Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSHUFHW;
  Decode_Vn_K_z_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSHUFHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPCMPEQB_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPCMPEQB  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPCMPEQB;
  Decode_rK_K_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPCMPEQW_rK_K_Hn_Wn(PInst: PInstruction);
begin
  // VPCMPEQW  rK{K},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPCMPEQW;
  Decode_rK_K_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPCMPEQW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VMOVDQU16_Wn_K_z_Vn(PInst: PInstruction);
begin
  // VMOVDQU16  Wn{K}{z},Vn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VMOVDQU16;
  Decode_Wn_K_z_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQU16);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VMOVDQU8_Wn_K_z_Vn(PInst: PInstruction);
begin
  // VMOVDQU8  Wn{K}{z},Vn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VMOVDQU8;
  Decode_Wn_K_z_Vn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMOVDQU8);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSRLW_Vn_K_z_Hn_Wo_b(PInst: PInstruction);
begin
  // VPSRLW  Vn{K}{z},Hn,Wo.b
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_M128);
  PInst^.InstID := INST_ID_VPSRLW;
  Decode_Vn_K_z_Hn_Wo_b(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMULLW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMULLW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMULLW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSUBUSB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSUBUSB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSUBUSB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSUBUSW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSUBUSW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSUBUSW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBUSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMINUB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMINUB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMINUB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPADDUSB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPADDUSB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPADDUSB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPADDUSW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPADDUSW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPADDUSW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDUSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMAXUB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMAXUB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMAXUB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPAVGB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPAVGB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPAVGB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPAVGB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSRAW_Vn_K_z_Hn_Wo_b(PInst: PInstruction);
begin
  // VPSRAW  Vn{K}{z},Hn,Wo.b
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_M128);
  PInst^.InstID := INST_ID_VPSRAW;
  Decode_Vn_K_z_Hn_Wo_b(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPAVGW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPAVGW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPAVGW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPAVGW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMULHUW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMULHUW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMULHUW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMULHW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMULHW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMULHW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSUBSB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSUBSB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSUBSB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSUBSW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSUBSW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSUBSW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMINSW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMINSW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMINSW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMINSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPADDSB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPADDSB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPADDSB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPADDSW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPADDSW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPADDSW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMAXSW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMAXSW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMAXSW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMAXSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSLLW_Vn_K_z_Hn_Wo_b(PInst: PInstruction);
begin
  // VPSLLW  Vn{K}{z},Hn,Wo.b
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_M128);
  PInst^.InstID := INST_ID_VPSLLW;
  Decode_Vn_K_z_Hn_Wo_b(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPMADDWD_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPMADDWD  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPMADDWD;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMADDWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSADBW_Vn_Hn_Wn(PInst: PInstruction);
begin
  // VPSADBW  Vn,Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSADBW;
  Decode_Vn_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSUBB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSUBB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSUBB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSUBW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPSUBW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSUBW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPADDB_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPADDB  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPADDB;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPADDW_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  // VPADDW  Vn{K}{z},Hn,Wn
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPADDW;
  Decode_Vn_K_z_Hn_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSRLW_Hn_K_z_Wn_Ib(PInst: PInstruction);
begin
  // VPSRLW  Hn{K}{z},Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSRLW;
  Decode_Hn_K_z_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSRAW_Hn_K_z_Wn_Ib(PInst: PInstruction);
begin
  // VPSRAW  Hn{K}{z},Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSRAW;
  Decode_Hn_K_z_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSLLW_Hn_K_z_Wn_Ib(PInst: PInstruction);
begin
  // VPSLLW  Hn{K}{z},Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSLLW;
  Decode_Hn_K_z_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSRLDQ_Hn_Wn_Ib(PInst: PInstruction);
begin
  // VPSRLDQ  Hn,Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSRLDQ;
  Decode_Hn_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSRLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VL_VPSLLDQ_Hn_Wn_Ib(PInst: PInstruction);
begin
  // VPSLLDQ  Hn,Wn,Ib
  PInst^.InstGroups := (INST_GRP_AVX512BW or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FVM);
  PInst^.InstID := INST_ID_VPSLLDQ;
  Decode_Hn_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPSLLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
