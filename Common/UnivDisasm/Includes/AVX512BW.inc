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
// The Original Code is AVX512BW.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX512BW_VPEXTRB_Mb_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRB  Mb,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPEXTRB;
  Decode_Mb_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VPEXTRB_Ry_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRB  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPEXTRB;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VPEXTRW_Mw_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRW  Mw,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPEXTRW;
  Decode_Mw_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VPEXTRW_Ry_Vo_Ib(PInst: PInstruction);
begin
  // VPEXTRW  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPEXTRW;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VPINSRB_Vo_Ho_Mb_Ib(PInst: PInstruction);
begin
  // VPINSRB  Vo,Ho,Mb,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPINSRB;
  Decode_Vo_Ho_Mb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VPINSRB_Vo_Ho_Rd_Ib(PInst: PInstruction);
begin
  // VPINSRB  Vo,Ho,Rd,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPINSRB;
  Decode_Vo_Ho_Rd_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KSHIFTRQ_rK_mK_Ib(PInst: PInstruction);
begin
  // KSHIFTRQ  rK,mK,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KSHIFTRQ;
  Decode_rK_mK_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KSHIFTRQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KSHIFTRD_rK_mK_Ib(PInst: PInstruction);
begin
  // KSHIFTRD  rK,mK,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KSHIFTRD;
  Decode_rK_mK_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KSHIFTRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KSHIFTLQ_rK_mK_Ib(PInst: PInstruction);
begin
  // KSHIFTLQ  rK,mK,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KSHIFTLQ;
  Decode_rK_mK_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KSHIFTLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KSHIFTLD_rK_mK_Ib(PInst: PInstruction);
begin
  // KSHIFTLD  rK,mK,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KSHIFTLD;
  Decode_rK_mK_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KSHIFTLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KANDD_rK_vK_mK(PInst: PInstruction);
begin
  // KANDD  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KANDD;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KANDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KANDQ_rK_vK_mK(PInst: PInstruction);
begin
  // KANDQ  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KANDQ;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KANDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KANDND_rK_vK_mK(PInst: PInstruction);
begin
  // KANDND  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KANDND;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KANDND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KANDNQ_rK_vK_mK(PInst: PInstruction);
begin
  // KANDNQ  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KANDNQ;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KANDNQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KNOTD_rK_mK(PInst: PInstruction);
begin
  // KNOTD  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KNOTD;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KNOTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KNOTQ_rK_mK(PInst: PInstruction);
begin
  // KNOTQ  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KNOTQ;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KNOTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KORD_rK_vK_mK(PInst: PInstruction);
begin
  // KORD  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KORD;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KORD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KORQ_rK_vK_mK(PInst: PInstruction);
begin
  // KORQ  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KORQ;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KORQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KXNORD_rK_vK_mK(PInst: PInstruction);
begin
  // KXNORD  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KXNORD;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KXNORD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KXNORQ_rK_vK_mK(PInst: PInstruction);
begin
  // KXNORQ  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KXNORQ;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KXNORQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KXORD_rK_vK_mK(PInst: PInstruction);
begin
  // KXORD  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KXORD;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KXORD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KXORQ_rK_vK_mK(PInst: PInstruction);
begin
  // KXORQ  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KXORQ;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KXORQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KADDD_rK_vK_mK(PInst: PInstruction);
begin
  // KADDD  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KADDD;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KADDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KADDQ_rK_vK_mK(PInst: PInstruction);
begin
  // KADDQ  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KADDQ;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KADDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KUNPCKDQ_rK_vK_mK(PInst: PInstruction);
begin
  // KUNPCKDQ  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KUNPCKDQ;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KUNPCKDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KUNPCKWD_rK_vK_mK(PInst: PInstruction);
begin
  // KUNPCKWD  rK,vK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KUNPCKWD;
  Decode_rK_vK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KUNPCKWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVD_rK_Md(PInst: PInstruction);
begin
  // KMOVD  rK,Md
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KMOVD;
  Decode_rK_Md(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVD_rK_mK(PInst: PInstruction);
begin
  // KMOVD  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KMOVD;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVQ_rK_Mq(PInst: PInstruction);
begin
  // KMOVQ  rK,Mq
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KMOVQ;
  Decode_rK_Mq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVQ_rK_mK(PInst: PInstruction);
begin
  // KMOVQ  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KMOVQ;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVD_Md_rK(PInst: PInstruction);
begin
  // KMOVD  Md,rK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KMOVD;
  Decode_Md_rK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVQ_Mq_rK(PInst: PInstruction);
begin
  // KMOVQ  Mq,rK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KMOVQ;
  Decode_Mq_rK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVQ_rK_Rq(PInst: PInstruction);
begin
  // KMOVQ  rK,Rq
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_KMOVQ;
  Decode_rK_Rq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVD_rK_Ry(PInst: PInstruction);
begin
  // KMOVD  rK,Ry
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_KMOVD;
  Decode_rK_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVQ_Gq_mK(PInst: PInstruction);
begin
  // KMOVQ  Gq,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_KMOVQ;
  Decode_Gq_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KMOVD_Gy_mK(PInst: PInstruction);
begin
  // KMOVD  Gy,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_KMOVD;
  Decode_Gy_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KMOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KORTESTD_rK_mK(PInst: PInstruction);
begin
  // KORTESTD  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KORTESTD;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KORTESTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KORTESTQ_rK_mK(PInst: PInstruction);
begin
  // KORTESTQ  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KORTESTQ;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KORTESTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KTESTD_rK_mK(PInst: PInstruction);
begin
  // KTESTD  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_KTESTD;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KTESTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_KTESTQ_rK_mK(PInst: PInstruction);
begin
  // KTESTQ  rK,mK
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_KTESTQ;
  Decode_rK_mK(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_KTESTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VPINSRW_Vo_Ho_Mw_Ib(PInst: PInstruction);
begin
  // VPINSRW  Vo,Ho,Mw,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPINSRW;
  Decode_Vo_Ho_Mw_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VPINSRW_Vo_Ho_Rv_Ib(PInst: PInstruction);
begin
  // VPINSRW  Vo,Ho,Rv,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetTuple4VL128(TT_T1S);
  PInst^.InstID := INST_ID_VPINSRW;
  Decode_Vo_Ho_Rv_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPINSRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX512BW_VPEXTRW_Gy_Uo_Ib(PInst: PInstruction);
begin
  // VPEXTRW  Gy,Uo,Ib
  PInst^.InstGroups := INST_GRP_AVX512BW;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPEXTRW;
  Decode_Gy_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
