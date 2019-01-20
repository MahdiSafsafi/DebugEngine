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
// The Original Code is SSE2.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SSE2_MOVSD_Vo_Mo_q(PInst: PInstruction);
begin
  // MOVSD  Vo,Mo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MOVSD;
  Decode_Vo_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVSD_Vo_Uo(PInst: PInstruction);
begin
  // MOVSD  Vo,Uo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MOVSD;
  Decode_Vo_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVUPD_Vo_Wo(PInst: PInstruction);
begin
  // MOVUPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED or INST_CATEGORY_UNALIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVUPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVUPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVSD_Mo_q_Vo(PInst: PInstruction);
begin
  // MOVSD  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MOVSD;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVSD_Uo_Vo(PInst: PInstruction);
begin
  // MOVSD  Uo,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MOVSD;
  Decode_Uo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVUPD_Wo_Vo(PInst: PInstruction);
begin
  // MOVUPD  Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED or INST_CATEGORY_UNALIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVUPD;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVUPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVLPD_Vo_Mo_q(PInst: PInstruction);
begin
  // MOVLPD  Vo,Mo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVLPD;
  Decode_Vo_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVLPD_Mo_q_Vo(PInst: PInstruction);
begin
  // MOVLPD  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVLPD;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_UNPCKLPD_Vo_Wo(PInst: PInstruction);
begin
  // UNPCKLPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_UNPCKLPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_UNPCKLPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_UNPCKHPD_Vo_Wo(PInst: PInstruction);
begin
  // UNPCKHPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_UNPCKHPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_UNPCKHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVHPD_Vo_Mo_q(PInst: PInstruction);
begin
  // MOVHPD  Vo,Mo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVHPD;
  Decode_Vo_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVHPD_Mo_q_Vo(PInst: PInstruction);
begin
  // MOVHPD  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVHPD;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVHPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVAPD_Vo_Wo(PInst: PInstruction);
begin
  // MOVAPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ALIGNED or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVAPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVAPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVAPD_Wo_Vo(PInst: PInstruction);
begin
  // MOVAPD  Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ALIGNED or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVAPD;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVAPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTPI2PD_Vo_Mq(PInst: PInstruction);
begin
  // CVTPI2PD  Vo,Mq
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CVTPI2PD;
  Decode_Vo_Mq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPI2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTSI2SD_Vo_Ey(PInst: PInstruction);
begin
  // CVTSI2SD  Vo,Ey
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_CVTSI2SD;
  Decode_Vo_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTSI2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVNTPD_Mo_Vo(PInst: PInstruction);
begin
  // MOVNTPD  Mo,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED or INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVNTPD;
  Decode_Mo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVNTPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTTSD2SI_Gy_Wo_q(PInst: PInstruction);
begin
  // CVTTSD2SI  Gy,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_CVTTSD2SI;
  Decode_Gy_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTTSD2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTSD2SI_Gy_Wo_q(PInst: PInstruction);
begin
  // CVTSD2SI  Gy,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_CVTSD2SI;
  Decode_Gy_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTSD2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_UCOMISD_Vo_Wo_q(PInst: PInstruction);
begin
  // UCOMISD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_UCOMISD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_UCOMISD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_COMISD_Vo_Wo_q(PInst: PInstruction);
begin
  // COMISD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_COMISD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_COMISD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVMSKPD_Gy_Uo(PInst: PInstruction);
begin
  // MOVMSKPD  Gy,Uo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVMSKPD;
  Decode_Gy_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVMSKPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_SQRTPD_Vo_Wo(PInst: PInstruction);
begin
  // SQRTPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_SQRTPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SQRTPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_SQRTSD_Vo_Wo_q(PInst: PInstruction);
begin
  // SQRTSD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_SQRTSD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SQRTSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_ANDPD_Vo_Wo(PInst: PInstruction);
begin
  // ANDPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOGICAL or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ANDPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ANDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_ANDNPD_Vo_Wo(PInst: PInstruction);
begin
  // ANDNPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOGICAL or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ANDNPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ANDNPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_ORPD_Vo_Wo(PInst: PInstruction);
begin
  // ORPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOGICAL or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ORPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ORPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_XORPD_Vo_Wo(PInst: PInstruction);
begin
  // XORPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOGICAL or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_XORPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XORPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_ADDPD_Vo_Wo(PInst: PInstruction);
begin
  // ADDPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ADDPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_ADDSD_Vo_Wo_q(PInst: PInstruction);
begin
  // ADDSD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_ADDSD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MULPD_Vo_Wo(PInst: PInstruction);
begin
  // MULPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MULPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MULPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MULSD_Vo_Wo_q(PInst: PInstruction);
begin
  // MULSD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MULSD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MULSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTPD2PS_Vo_Wo(PInst: PInstruction);
begin
  // CVTPD2PS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CVTPD2PS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPD2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTSD2SS_Vo_Wo_q(PInst: PInstruction);
begin
  // CVTSD2SS  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_CVTSD2SS;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTSD2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTSS2SD_Vo_Wo_d(PInst: PInstruction);
begin
  // CVTSS2SD  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_CVTSS2SD;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTSS2SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTPS2PD_Vo_Wo_q(PInst: PInstruction);
begin
  // CVTPS2PD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CVTPS2PD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPS2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTPS2DQ_Vo_Wo(PInst: PInstruction);
begin
  // CVTPS2DQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CVTPS2DQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPS2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTTPS2DQ_Vo_Wo(PInst: PInstruction);
begin
  // CVTTPS2DQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_CVTTPS2DQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTTPS2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTDQ2PS_Vo_Wo(PInst: PInstruction);
begin
  // CVTDQ2PS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CVTDQ2PS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTDQ2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_SUBPD_Vo_Wo(PInst: PInstruction);
begin
  // SUBPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_SUBPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_SUBSD_Vo_Wo_q(PInst: PInstruction);
begin
  // SUBSD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_SUBSD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUBSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MINPD_Vo_Wo(PInst: PInstruction);
begin
  // MINPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MINPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MINPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MINSD_Vo_Wo_q(PInst: PInstruction);
begin
  // MINSD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MINSD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MINSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_DIVPD_Vo_Wo(PInst: PInstruction);
begin
  // DIVPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_DIVPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DIVPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_DIVSD_Vo_Wo_q(PInst: PInstruction);
begin
  // DIVSD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_DIVSD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DIVSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MAXPD_Vo_Wo(PInst: PInstruction);
begin
  // MAXPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MAXPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MAXPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MAXSD_Vo_Wo_q(PInst: PInstruction);
begin
  // MAXSD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SCALAR);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MAXSD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MAXSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PUNPCKLBW_Vo_Wo(PInst: PInstruction);
begin
  // PUNPCKLBW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PUNPCKLBW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUNPCKLBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PUNPCKLWD_Vo_Wo(PInst: PInstruction);
begin
  // PUNPCKLWD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PUNPCKLWD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUNPCKLWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PUNPCKLDQ_Vo_Wo(PInst: PInstruction);
begin
  // PUNPCKLDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PUNPCKLDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUNPCKLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PACKSSWB_Vo_Wo(PInst: PInstruction);
begin
  // PACKSSWB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_PACK or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PACKSSWB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PACKSSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PCMPGTB_Vo_Wo(PInst: PInstruction);
begin
  // PCMPGTB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPGTB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPGTB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PCMPGTW_Vo_Wo(PInst: PInstruction);
begin
  // PCMPGTW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPGTW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPGTW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PCMPGTD_Vo_Wo(PInst: PInstruction);
begin
  // PCMPGTD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPGTD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPGTD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PACKUSWB_Vo_Wo(PInst: PInstruction);
begin
  // PACKUSWB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_PACK or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PACKUSWB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PACKUSWB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PUNPCKHBW_Vo_Wo(PInst: PInstruction);
begin
  // PUNPCKHBW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PUNPCKHBW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUNPCKHBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PUNPCKHWD_Vo_Wo(PInst: PInstruction);
begin
  // PUNPCKHWD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PUNPCKHWD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUNPCKHWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PUNPCKHDQ_Vo_Wo(PInst: PInstruction);
begin
  // PUNPCKHDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PUNPCKHDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUNPCKHDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PACKSSDW_Vo_Wo(PInst: PInstruction);
begin
  // PACKSSDW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_PACK or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PACKSSDW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PACKSSDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PUNPCKLQDQ_Vo_Wo(PInst: PInstruction);
begin
  // PUNPCKLQDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PUNPCKLQDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUNPCKLQDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PUNPCKHQDQ_Vo_Wo(PInst: PInstruction);
begin
  // PUNPCKHQDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PUNPCKHQDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUNPCKHQDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVQ_Vo_Eq(PInst: PInstruction);
begin
  // MOVQ  Vo,Eq
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVQ;
  Decode_Vo_Eq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVD_Vo_Ed(PInst: PInstruction);
begin
  // MOVD  Vo,Ed
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVD;
  Decode_Vo_Ed(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVDQA_Vo_Wo(PInst: PInstruction);
begin
  // MOVDQA  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_ALIGNED or
    INST_CATEGORY_INTEGER or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVDQA;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVDQU_Vo_Wo(PInst: PInstruction);
begin
  // MOVDQU  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD or INST_CATEGORY_UNALIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVDQU;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSHUFD_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PSHUFD  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SHUFFLE or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSHUFD;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHUFD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSHUFLW_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PSHUFLW  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SHUFFLE or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_PSHUFLW;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHUFLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSHUFHW_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PSHUFHW  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SHUFFLE or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_PSHUFHW;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHUFHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PCMPEQB_Vo_Wo(PInst: PInstruction);
begin
  // PCMPEQB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPEQB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPEQB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PCMPEQW_Vo_Wo(PInst: PInstruction);
begin
  // PCMPEQW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPEQW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPEQW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PCMPEQD_Vo_Wo(PInst: PInstruction);
begin
  // PCMPEQD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPEQD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPEQD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVQ_Eq_Vo(PInst: PInstruction);
begin
  // MOVQ  Eq,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVQ;
  Decode_Eq_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVD_Ed_Vo(PInst: PInstruction);
begin
  // MOVD  Ed,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVD;
  Decode_Ed_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVQ_Vo_Wo_q(PInst: PInstruction);
begin
  // MOVQ  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVQ;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVDQA_Wo_Vo(PInst: PInstruction);
begin
  // MOVDQA  Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_ALIGNED or
    INST_CATEGORY_INTEGER or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVDQA;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVDQU_Wo_Vo(PInst: PInstruction);
begin
  // MOVDQU  Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD or INST_CATEGORY_UNALIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVDQU;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CMPccPD_Vo_Wo(PInst: PInstruction);
begin
  // CMPccPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  Decode_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_CMPccPD_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_CMPccPD_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CMPccSD_Vo_Wo_q(PInst: PInstruction);
begin
  // CMPccSD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  Decode_Vo_Wo_q(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_CMPccSD_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_CMPccSD_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PINSRW_Vo_Mw_Ib(PInst: PInstruction);
begin
  // PINSRW  Vo,Mw,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PINSRW;
  Decode_Vo_Mw_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PINSRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PINSRW_Vo_Rv_Ib(PInst: PInstruction);
begin
  // PINSRW  Vo,Rv,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PINSRW;
  Decode_Vo_Rv_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PINSRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PEXTRW_Gy_Uo_Ib(PInst: PInstruction);
begin
  // PEXTRW  Gy,Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_EXTRACTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PEXTRW;
  Decode_Gy_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_SHUFPD_Vo_Wo_Ib(PInst: PInstruction);
begin
  // SHUFPD  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SHUFFLE);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_SHUFPD;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHUFPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRLW_Vo_Wo(PInst: PInstruction);
begin
  // PSRLW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRLW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRLD_Vo_Wo(PInst: PInstruction);
begin
  // PSRLD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRLD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRLQ_Vo_Wo(PInst: PInstruction);
begin
  // PSRLQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRLQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PADDQ_Vo_Wo(PInst: PInstruction);
begin
  // PADDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PADDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMULLW_Vo_Wo(PInst: PInstruction);
begin
  // PMULLW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED or INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMULLW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVQ_Wo_q_Vo(PInst: PInstruction);
begin
  // MOVQ  Wo.q,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVQ;
  Decode_Wo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMOVMSKB_Gy_Uo(PInst: PInstruction);
begin
  // PMOVMSKB  Gy,Uo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVMSKB;
  Decode_Gy_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVMSKB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSUBUSB_Vo_Wo(PInst: PInstruction);
begin
  // PSUBUSB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSUBUSB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSUBUSW_Vo_Wo(PInst: PInstruction);
begin
  // PSUBUSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSUBUSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBUSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMINUB_Vo_Wo(PInst: PInstruction);
begin
  // PMINUB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMINUB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMINUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PAND_Vo_Wo(PInst: PInstruction);
begin
  // PAND  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PAND;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PAND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PADDUSB_Vo_Wo(PInst: PInstruction);
begin
  // PADDUSB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PADDUSB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDUSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PADDUSW_Vo_Wo(PInst: PInstruction);
begin
  // PADDUSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PADDUSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDUSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMAXUB_Vo_Wo(PInst: PInstruction);
begin
  // PMAXUB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMAXUB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMAXUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PANDN_Vo_Wo(PInst: PInstruction);
begin
  // PANDN  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PANDN;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PANDN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PAVGB_Vo_Wo(PInst: PInstruction);
begin
  // PAVGB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PAVGB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PAVGB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRAW_Vo_Wo(PInst: PInstruction);
begin
  // PSRAW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRAW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRAD_Vo_Wo(PInst: PInstruction);
begin
  // PSRAD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRAD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PAVGW_Vo_Wo(PInst: PInstruction);
begin
  // PAVGW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PAVGW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PAVGW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMULHUW_Vo_Wo(PInst: PInstruction);
begin
  // PMULHUW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_STORE or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMULHUW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULHUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMULHW_Vo_Wo(PInst: PInstruction);
begin
  // PMULHW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED or INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMULHW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTTPD2DQ_Vo_Wo(PInst: PInstruction);
begin
  // CVTTPD2DQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CVTTPD2DQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTTPD2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTPD2DQ_Vo_Wo(PInst: PInstruction);
begin
  // CVTPD2DQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_CVTPD2DQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPD2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CVTDQ2PD_Vo_Wo_q(PInst: PInstruction);
begin
  // CVTDQ2PD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_CVTDQ2PD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTDQ2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MOVNTDQ_Mo_Vo(PInst: PInstruction);
begin
  // MOVNTDQ  Mo,Vo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVNTDQ;
  Decode_Mo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVNTDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSUBSB_Vo_Wo(PInst: PInstruction);
begin
  // PSUBSB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSUBSB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSUBSW_Vo_Wo(PInst: PInstruction);
begin
  // PSUBSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSUBSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMINSW_Vo_Wo(PInst: PInstruction);
begin
  // PMINSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMINSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMINSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_POR_Vo_Wo(PInst: PInstruction);
begin
  // POR  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_POR;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PADDSB_Vo_Wo(PInst: PInstruction);
begin
  // PADDSB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PADDSB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PADDSW_Vo_Wo(PInst: PInstruction);
begin
  // PADDSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PADDSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMAXSW_Vo_Wo(PInst: PInstruction);
begin
  // PMAXSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMAXSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMAXSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PXOR_Vo_Wo(PInst: PInstruction);
begin
  // PXOR  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PXOR;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PXOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSLLW_Vo_Wo(PInst: PInstruction);
begin
  // PSLLW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSLLW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSLLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSLLD_Vo_Wo(PInst: PInstruction);
begin
  // PSLLD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSLLD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSLLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSLLQ_Vo_Wo(PInst: PInstruction);
begin
  // PSLLQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSLLQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSLLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMULUDQ_Vo_Wo(PInst: PInstruction);
begin
  // PMULUDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMULUDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULUDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PMADDWD_Vo_Wo(PInst: PInstruction);
begin
  // PMADDWD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMADDWD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMADDWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSADBW_Vo_Wo(PInst: PInstruction);
begin
  // PSADBW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSADBW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_MASKMOVDQU_Vo_Uo(PInst: PInstruction);
begin
  // MASKMOVDQU  Vo,Uo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MASKMOVDQU;
  Decode_Vo_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MASKMOVDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSUBB_Vo_Wo(PInst: PInstruction);
begin
  // PSUBB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSUBB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSUBW_Vo_Wo(PInst: PInstruction);
begin
  // PSUBW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSUBW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSUBD_Vo_Wo(PInst: PInstruction);
begin
  // PSUBD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSUBD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSUBQ_Vo_Wo(PInst: PInstruction);
begin
  // PSUBQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSUBQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PADDB_Vo_Wo(PInst: PInstruction);
begin
  // PADDB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PADDB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PADDW_Vo_Wo(PInst: PInstruction);
begin
  // PADDW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PADDW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PADDD_Vo_Wo(PInst: PInstruction);
begin
  // PADDD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PADDD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRLW_Uo_Ib(PInst: PInstruction);
begin
  // PSRLW  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRLW;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRAW_Uo_Ib(PInst: PInstruction);
begin
  // PSRAW  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRAW;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRAW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSLLW_Uo_Ib(PInst: PInstruction);
begin
  // PSLLW  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSLLW;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSLLW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRLD_Uo_Ib(PInst: PInstruction);
begin
  // PSRLD  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRLD;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRAD_Uo_Ib(PInst: PInstruction);
begin
  // PSRAD  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRAD;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSLLD_Uo_Ib(PInst: PInstruction);
begin
  // PSLLD  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSLLD;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSLLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRLQ_Uo_Ib(PInst: PInstruction);
begin
  // PSRLQ  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRLQ;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSRLDQ_Uo_Ib(PInst: PInstruction);
begin
  // PSRLDQ  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SHIFT or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSRLDQ;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSRLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSLLQ_Uo_Ib(PInst: PInstruction);
begin
  // PSLLQ  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SHIFT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSLLQ;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSLLQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_PSLLDQ_Uo_Ib(PInst: PInstruction);
begin
  // PSLLDQ  Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SHIFT or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSLLDQ;
  Decode_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSLLDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE2_CLFLUSH_M(PInst: PInstruction);
begin
  // CLFLUSH  M
  PInst^.InstGroups := INST_GRP_SSE2;
  PInst^.InstCategory := INST_CATEGORY_CACHEABILITY_CONTROL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CLFLUSH;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CLFLUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
