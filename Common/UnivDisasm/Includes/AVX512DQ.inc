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
// The Original Code is AVX512DQ.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512DQ_VBROADCASTF32X8_Vz_K_z_My(PInst: PInstruction);
begin
  // VBROADCASTF32X8  Vz{K}{z},My
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T8);
  PInst^.InstID := INST_ID_VBROADCASTF32X8;
  Decode_Vz_K_z_My(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTF32X8);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VBROADCASTI32X8_Vz_K_z_My(PInst: PInstruction);
begin
  // VBROADCASTI32X8  Vz{K}{z},My
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T8);
  PInst^.InstID := INST_ID_VBROADCASTI32X8;
  Decode_Vz_K_z_My(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VBROADCASTI32X8);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VPEXTRQ_Eq_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRQ  Eq,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPEXTRQ;
  Decode_Eq_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VPEXTRD_Ed_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRD  Ed,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPEXTRD;
  Decode_Ed_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VINSERTF32X8_Vz_K_z_Hz_Wy_Ib(PInst: PInstruction);
begin
  // VINSERTF32X8  Vz{K}{z},Hz,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T8);
  PInst^.InstID := INST_ID_VINSERTF32X8;
  Decode_Vz_K_z_Hz_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTF32X8);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VEXTRACTF32X8_Wy_K_z_Vz_Ib(PInst: PInstruction);
begin
  // VEXTRACTF32X8  Wy{K}{z},Vz,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T8);
  PInst^.InstID := INST_ID_VEXTRACTF32X8;
  Decode_Wy_K_z_Vz_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTF32X8);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VPINSRQ_Vo_Ho_Eq_Ib(PInst: PInstruction);
begin
  // VPINSRQ  Vo,Ho,Eq,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPINSRQ;
  Decode_Vo_Ho_Eq_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VPINSRD_Vo_Ho_Ed_Ib(PInst: PInstruction);
begin
  // VPINSRD  Vo,Ho,Ed,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPINSRD;
  Decode_Vo_Ho_Ed_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KSHIFTRB_rK_mK_Ib(PInst: PInstruction);
begin
  // KSHIFTRB  rK,mK,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KSHIFTRB;
  Decode_rK_mK_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KSHIFTRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KSHIFTLB_rK_mK_Ib(PInst: PInstruction);
begin
  // KSHIFTLB  rK,mK,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KSHIFTLB;
  Decode_rK_mK_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KSHIFTLB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VINSERTI32X8_Vz_K_z_Hz_Wy_Ib(PInst: PInstruction);
begin
  // VINSERTI32X8  Vz{K}{z},Hz,Wy,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T8);
  PInst^.InstID := INST_ID_VINSERTI32X8;
  Decode_Vz_K_z_Hz_Wy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VINSERTI32X8);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VEXTRACTI32X8_Wy_K_z_Vz_Ib(PInst: PInstruction);
begin
  // VEXTRACTI32X8  Wy{K}{z},Vz,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL512(TT_T8);
  PInst^.InstID := INST_ID_VEXTRACTI32X8;
  Decode_Wy_K_z_Vz_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VEXTRACTI32X8);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VRANGESD_Vo_K_z_Ho_Wo_q_sae_Ib(PInst: PInstruction);
begin
  // VRANGESD  Vo{K}{z},Ho,Wo.q{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRANGESD;
  Decode_Vo_K_z_Ho_Wo_q_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRANGESD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VRANGESS_Vo_K_z_Ho_Wo_d_sae_Ib(PInst: PInstruction);
begin
  // VRANGESS  Vo{K}{z},Ho,Wo.d{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VRANGESS;
  Decode_Vo_K_z_Ho_Wo_d_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VRANGESS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VREDUCESD_Vo_K_z_Ho_Wo_q_sae_Ib(PInst: PInstruction);
begin
  // VREDUCESD  Vo{K}{z},Ho,Wo.q{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VREDUCESD;
  Decode_Vo_K_z_Ho_Wo_q_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VREDUCESD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VREDUCESS_Vo_K_z_Ho_Wo_d_sae_Ib(PInst: PInstruction);
begin
  // VREDUCESS  Vo{K}{z},Ho,Wo.d{sae},Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VREDUCESS;
  Decode_Vo_K_z_Ho_Wo_d_sae_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VREDUCESS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VFPCLASSSD_rK_K_Wo_q_Ib(PInst: PInstruction);
begin
  // VFPCLASSSD  rK{K},Wo.q,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFPCLASSSD;
  Decode_rK_K_Wo_q_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFPCLASSSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_VFPCLASSSS_rK_K_Wo_d_Ib(PInst: PInstruction);
begin
  // VFPCLASSSS  rK{K},Wo.d,Ib
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VFPCLASSSS;
  Decode_rK_K_Wo_d_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFPCLASSSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KANDB_rK_vK_mK(PInst: PInstruction);
begin
  // KANDB  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KANDB;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KANDB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KANDNB_rK_vK_mK(PInst: PInstruction);
begin
  // KANDNB  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KANDNB;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KANDNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KNOTB_rK_mK(PInst: PInstruction);
begin
  // KNOTB  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KNOTB;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KNOTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KORB_rK_vK_mK(PInst: PInstruction);
begin
  // KORB  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KORB;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KORB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KXNORB_rK_vK_mK(PInst: PInstruction);
begin
  // KXNORB  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KXNORB;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KXNORB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KXORB_rK_vK_mK(PInst: PInstruction);
begin
  // KXORB  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KXORB;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KXORB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KADDB_rK_vK_mK(PInst: PInstruction);
begin
  // KADDB  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KADDB;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KADDB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KADDW_rK_vK_mK(PInst: PInstruction);
begin
  // KADDW  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KADDW;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KMOVB_rK_Mb(PInst: PInstruction);
begin
  // KMOVB  rK,Mb
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KMOVB;
  Decode_rK_Mb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KMOVB_rK_mK(PInst: PInstruction);
begin
  // KMOVB  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KMOVB;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KMOVB_Mb_rK(PInst: PInstruction);
begin
  // KMOVB  Mb,rK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KMOVB;
  Decode_Mb_rK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KMOVB_rK_Ry(PInst: PInstruction);
begin
  // KMOVB  rK,Ry
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KMOVB;
  Decode_rK_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KMOVB_Gy_mK(PInst: PInstruction);
begin
  // KMOVB  Gy,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KMOVB;
  Decode_Gy_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KORTESTB_rK_mK(PInst: PInstruction);
begin
  // KORTESTB  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KORTESTB;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KORTESTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KTESTB_rK_mK(PInst: PInstruction);
begin
  // KTESTB  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KTESTB;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KTESTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512DQ_KTESTW_rK_mK(PInst: PInstruction);
begin
  // KTESTW  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512DQ;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KTESTW;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KTESTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
