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
// The Original Code is AVX512DQ-VL.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512DQ_VL_VBROADCASTF32X2_Vu_K_z_Wo_q(PInst: PInstruction);
begin
  // VBROADCASTF32X2  Vu{K}{z},Wo.q
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T2);
  PInst^.SetTuple4VL256(TT_T2);
  PInst^.InstID := INST_ID_VBROADCASTF32X2;
  Decode_Vu_K_z_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTF32X2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VBROADCASTF64X2_Vu_K_z_Mo(PInst: PInstruction);
begin
  // VBROADCASTF64X2  Vu{K}{z},Mo
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL256(TT_T2);
  PInst^.SetTuple4VL512(TT_T2);
  PInst^.InstID := INST_ID_VBROADCASTF64X2;
  Decode_Vu_K_z_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTF64X2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VPMOVM2Q_Vn_mK(PInst: PInstruction);
begin
  // VPMOVM2Q  Vn,mK
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPMOVM2Q;
  Decode_Vn_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVM2Q);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VPMOVM2D_Vn_mK(PInst: PInstruction);
begin
  // VPMOVM2D  Vn,mK
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPMOVM2D;
  Decode_Vn_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVM2D);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VPMOVQ2M_rK_Un(PInst: PInstruction);
begin
  // VPMOVQ2M  rK,Un
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPMOVQ2M;
  Decode_rK_Un(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVQ2M);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VPMOVD2M_rK_Un(PInst: PInstruction);
begin
  // VPMOVD2M  rK,Un
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VPMOVD2M;
  Decode_rK_Un(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMOVD2M);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VPMULLQ_Vn_K_z_Hn_B64_Wn(PInst: PInstruction);
begin
  // VPMULLQ  Vn{K}{z},Hn,B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VPMULLQ;
  Decode_Vn_K_z_Hn_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPMULLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VBROADCASTI32X2_Vn_K_z_Wo_q(PInst: PInstruction);
begin
  // VBROADCASTI32X2  Vn{K}{z},Wo.q
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_T2);
  PInst^.InstID := INST_ID_VBROADCASTI32X2;
  Decode_Vn_K_z_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTI32X2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VBROADCASTI64X2_Vu_K_z_Mo(PInst: PInstruction);
begin
  // VBROADCASTI64X2  Vu{K}{z},Mo
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T2);
  PInst^.SetTuple4VL256(TT_T2);
  PInst^.InstID := INST_ID_VBROADCASTI64X2;
  Decode_Vu_K_z_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTI64X2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VINSERTF64X2_Vu_K_z_Hu_Wo_Ib(PInst: PInstruction);
begin
  // VINSERTF64X2  Vu{K}{z},Hu,Wo,Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL256(TT_T2);
  PInst^.SetTuple4VL512(TT_T2);
  PInst^.InstID := INST_ID_VINSERTF64X2;
  Decode_Vu_K_z_Hu_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTF64X2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VEXTRACTF64X2_Wo_K_z_Vu_Ib(PInst: PInstruction);
begin
  // VEXTRACTF64X2  Wo{K}{z},Vu,Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL256(TT_T2);
  PInst^.SetTuple4VL512(TT_T2);
  PInst^.InstID := INST_ID_VEXTRACTF64X2;
  Decode_Wo_K_z_Vu_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTF64X2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VINSERTI64X2_Vu_K_z_Hu_Wo_Ib(PInst: PInstruction);
begin
  // VINSERTI64X2  Vu{K}{z},Hu,Wo,Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL256(TT_T2);
  PInst^.SetTuple4VL512(TT_T2);
  PInst^.InstID := INST_ID_VINSERTI64X2;
  Decode_Vu_K_z_Hu_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTI64X2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VEXTRACTI64X2_Wo_K_z_Vu_Ib(PInst: PInstruction);
begin
  // VEXTRACTI64X2  Wo{K}{z},Vu,Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL256(TT_T2);
  PInst^.SetTuple4VL512(TT_T2);
  PInst^.InstID := INST_ID_VEXTRACTI64X2;
  Decode_Wo_K_z_Vu_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTI64X2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VRANGEPD_Vn_K_z_Hn_B64_Wn_sae_Ib
  (PInst: PInstruction);
begin
  // VRANGEPD  Vn{K}{z},Hn,B64(Wn){sae},Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VRANGEPD;
  Decode_Vn_K_z_Hn_B64_Wn_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRANGEPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VRANGEPS_Vn_K_z_Hn_B32_Wn_sae_Ib
  (PInst: PInstruction);
begin
  // VRANGEPS  Vn{K}{z},Hn,B32(Wn){sae},Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VRANGEPS;
  Decode_Vn_K_z_Hn_B32_Wn_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRANGEPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VREDUCEPD_Vn_K_z_B64_Wn_sae_Ib
  (PInst: PInstruction);
begin
  // VREDUCEPD  Vn{K}{z},B64(Wn){sae},Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VREDUCEPD;
  Decode_Vn_K_z_B64_Wn_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VREDUCEPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VREDUCEPS_Vn_K_z_B32_Wn_sae_Ib
  (PInst: PInstruction);
begin
  // VREDUCEPS  Vn{K}{z},B32(Wn){sae},Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VREDUCEPS;
  Decode_Vn_K_z_B32_Wn_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VREDUCEPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VFPCLASSPD_rK_K_B64_Wn_Ib(PInst: PInstruction);
begin
  // VFPCLASSPD  rK{K},B64(Wn),Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VFPCLASSPD;
  Decode_rK_K_B64_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFPCLASSPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VFPCLASSPS_rK_K_B32_Wn_Ib(PInst: PInstruction);
begin
  // VFPCLASSPS  rK{K},B32(Wn),Ib
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VFPCLASSPS;
  Decode_rK_K_B32_Wn_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFPCLASSPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VANDPD_Vn_K_z_Hn_B64_Wn(PInst: PInstruction);
begin
  // VANDPD  Vn{K}{z},Hn,B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VANDPD;
  Decode_Vn_K_z_Hn_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VANDPS_Vn_K_z_Hn_B32_Wn(PInst: PInstruction);
begin
  // VANDPS  Vn{K}{z},Hn,B32(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VANDPS;
  Decode_Vn_K_z_Hn_B32_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VANDNPD_Vn_K_z_Hn_B64_Wn(PInst: PInstruction);
begin
  // VANDNPD  Vn{K}{z},Hn,B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VANDNPD;
  Decode_Vn_K_z_Hn_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDNPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VANDNPS_Vn_K_z_Hn_B32_Wn(PInst: PInstruction);
begin
  // VANDNPS  Vn{K}{z},Hn,B32(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VANDNPS;
  Decode_Vn_K_z_Hn_B32_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VANDNPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VORPD_Vn_K_z_Hn_B64_Wn(PInst: PInstruction);
begin
  // VORPD  Vn{K}{z},Hn,B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VORPD;
  Decode_Vn_K_z_Hn_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VORPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VORPS_Vn_K_z_Hn_B32_Wn(PInst: PInstruction);
begin
  // VORPS  Vn{K}{z},Hn,B32(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VORPS;
  Decode_Vn_K_z_Hn_B32_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VORPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VXORPD_Vn_K_z_Hn_B64_Wn(PInst: PInstruction);
begin
  // VXORPD  Vn{K}{z},Hn,B64(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VXORPD;
  Decode_Vn_K_z_Hn_B64_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VXORPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VXORPS_Vn_K_z_Hn_B32_Wn(PInst: PInstruction);
begin
  // VXORPS  Vn{K}{z},Hn,B32(Wn)
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VXORPS;
  Decode_Vn_K_z_Hn_B32_Wn(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VXORPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTQQ2PS_Vh_K_z_B64_Wn_er(PInst: PInstruction);
begin
  // VCVTQQ2PS  Vh{K}{z},B64(Wn){er}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VCVTQQ2PS;
  Decode_Vh_K_z_B64_Wn_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTQQ2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTTPD2UQQ_Vn_K_z_B64_Wn_sae(PInst: PInstruction);
begin
  // VCVTTPD2UQQ  Vn{K}{z},B64(Wn){sae}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VCVTTPD2UQQ;
  Decode_Vn_K_z_B64_Wn_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTPD2UQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTTPS2UQQ_Vn_K_z_B32_Wh_sae(PInst: PInstruction);
begin
  // VCVTTPS2UQQ  Vn{K}{z},B32(Wh){sae}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_HV);
  PInst^.InstID := INST_ID_VCVTTPS2UQQ;
  Decode_Vn_K_z_B32_Wh_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTPS2UQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTPD2UQQ_Vn_K_z_B64_Wn_er(PInst: PInstruction);
begin
  // VCVTPD2UQQ  Vn{K}{z},B64(Wn){er}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VCVTPD2UQQ;
  Decode_Vn_K_z_B64_Wn_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPD2UQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTPS2UQQ_Vn_K_z_B32_Wh_er(PInst: PInstruction);
begin
  // VCVTPS2UQQ  Vn{K}{z},B32(Wh){er}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_HV);
  PInst^.InstID := INST_ID_VCVTPS2UQQ;
  Decode_Vn_K_z_B32_Wh_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPS2UQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTTPD2QQ_Vn_K_z_B64_Wn_sae(PInst: PInstruction);
begin
  // VCVTTPD2QQ  Vn{K}{z},B64(Wn){sae}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VCVTTPD2QQ;
  Decode_Vn_K_z_B64_Wn_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTPD2QQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTUQQ2PS_Vh_K_z_B64_Wn_er(PInst: PInstruction);
begin
  // VCVTUQQ2PS  Vh{K}{z},B64(Wn){er}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VCVTUQQ2PS;
  Decode_Vh_K_z_B64_Wn_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTUQQ2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTUQQ2PD_Vn_K_z_B64_Wn_er(PInst: PInstruction);
begin
  // VCVTUQQ2PD  Vn{K}{z},B64(Wn){er}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VCVTUQQ2PD;
  Decode_Vn_K_z_B64_Wn_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTUQQ2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTTPS2QQ_Vn_K_z_B32_Wh_sae(PInst: PInstruction);
begin
  // VCVTTPS2QQ  Vn{K}{z},B32(Wh){sae}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_HV);
  PInst^.InstID := INST_ID_VCVTTPS2QQ;
  Decode_Vn_K_z_B32_Wh_sae(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTTPS2QQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTPD2QQ_Vn_K_z_B64_Wn_er(PInst: PInstruction);
begin
  // VCVTPD2QQ  Vn{K}{z},B64(Wn){er}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VCVTPD2QQ;
  Decode_Vn_K_z_B64_Wn_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPD2QQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTPS2QQ_Vn_K_z_B32_Wh_er(PInst: PInstruction);
begin
  // VCVTPS2QQ  Vn{K}{z},B32(Wh){er}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL(TT_HV);
  PInst^.InstID := INST_ID_VCVTPS2QQ;
  Decode_Vn_K_z_B32_Wh_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPS2QQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VL_VCVTQQ2PD_Vn_K_z_B64_Wn_er(PInst: PInstruction);
begin
  // VCVTQQ2PD  Vn{K}{z},B64(Wn){er}
  PInst^.InstGroups := (INST_GRP_AVX512DQ or INST_GRP_VL);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.SetTuple4VL(TT_FV);
  PInst^.InstID := INST_ID_VCVTQQ2PD;
  Decode_Vn_K_z_B64_Wn_er(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTQQ2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
