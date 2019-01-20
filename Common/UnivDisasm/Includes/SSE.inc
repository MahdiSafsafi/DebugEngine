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
// The Original Code is SSE.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SSE_PREFETCHx_M(PInst: PInstruction);
begin
  // PREFETCHx  M
  DecodeModRm(PInst);
  OpCodes_0x0F_0x0D_PREFETCH_Array[PInst^.ModRm.Value.Reg](PInst);
end;

procedure Decode_SSE_MOVSS_Vo_Mo_d(PInst: PInstruction);
begin
  // MOVSS  Vo,Mo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVSS;
  Decode_Vo_Mo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVSS_Vo_Uo(PInst: PInstruction);
begin
  // MOVSS  Vo,Uo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVSS;
  Decode_Vo_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVUPS_Vo_Wo(PInst: PInstruction);
begin
  // MOVUPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION or INST_CATEGORY_UNALIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVUPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVUPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVSS_Mo_d_Vo(PInst: PInstruction);
begin
  // MOVSS  Mo.d,Vo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVSS;
  Decode_Mo_d_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVSS_Uo_Vo(PInst: PInstruction);
begin
  // MOVSS  Uo,Vo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVSS;
  Decode_Uo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVUPS_Wo_Vo(PInst: PInstruction);
begin
  // MOVUPS  Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION or INST_CATEGORY_UNALIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVUPS;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVUPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVLPS_Vo_Mo_q(PInst: PInstruction);
begin
  // MOVLPS  Vo,Mo.q
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVLPS;
  Decode_Vo_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVHLPS_Vo_Uo(PInst: PInstruction);
begin
  // MOVHLPS  Vo,Uo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVHLPS;
  Decode_Vo_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVHLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVLPS_Mo_q_Vo(PInst: PInstruction);
begin
  // MOVLPS  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVLPS;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_UNPCKLPS_Vo_Wo(PInst: PInstruction);
begin
  // UNPCKLPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SINGLE_PRECISION or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_UNPCKLPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_UNPCKLPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_UNPCKHPS_Vo_Wo(PInst: PInstruction);
begin
  // UNPCKHPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_SINGLE_PRECISION or INST_CATEGORY_UNPACK);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_UNPCKHPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_UNPCKHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVHPS_Vo_Mo_q(PInst: PInstruction);
begin
  // MOVHPS  Vo,Mo.q
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVHPS;
  Decode_Vo_Mo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVLHPS_Vo_Uo(PInst: PInstruction);
begin
  // MOVLHPS  Vo,Uo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVLHPS;
  Decode_Vo_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVLHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVHPS_Mo_q_Vo(PInst: PInstruction);
begin
  // MOVHPS  Mo.q,Vo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVHPS;
  Decode_Mo_q_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVHPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVAPS_Vo_Wo(PInst: PInstruction);
begin
  // MOVAPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ALIGNED or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVAPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVAPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVAPS_Wo_Vo(PInst: PInstruction);
begin
  // MOVAPS  Wo,Vo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ALIGNED or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVAPS;
  Decode_Wo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVAPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_CVTPI2PS_Vo_Mq(PInst: PInstruction);
begin
  // CVTPI2PS  Vo,Mq
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CVTPI2PS;
  Decode_Vo_Mq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPI2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_CVTSI2SS_Vo_Ey(PInst: PInstruction);
begin
  // CVTSI2SS  Vo,Ey
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SCALAR or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_CVTSI2SS;
  Decode_Vo_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTSI2SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVNTPS_Mo_Vo(PInst: PInstruction);
begin
  // MOVNTPS  Mo,Vo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION or INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVNTPS;
  Decode_Mo_Vo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVNTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_CVTTSS2SI_Gy_Wo_d(PInst: PInstruction);
begin
  // CVTTSS2SI  Gy,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SCALAR or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_CVTTSS2SI;
  Decode_Gy_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTTSS2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_CVTSS2SI_Gy_Wo_d(PInst: PInstruction);
begin
  // CVTSS2SI  Gy,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SCALAR or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_CVTSS2SI;
  Decode_Gy_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTSS2SI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_UCOMISS_Vo_Wo_d(PInst: PInstruction);
begin
  // UCOMISS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $30;
  PInst^.InstID := INST_ID_UCOMISS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_UCOMISS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_COMISS_Vo_Wo_d(PInst: PInstruction);
begin
  // COMISS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $1A;
  PInst^.InstID := INST_ID_COMISS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_COMISS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MOVMSKPS_Gy_Uo(PInst: PInstruction);
begin
  // MOVMSKPS  Gy,Uo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVMSKPS;
  Decode_Gy_Uo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVMSKPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_SQRTSS_Vo_Wo_d(PInst: PInstruction);
begin
  // SQRTSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_SQRTSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SQRTSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_SQRTPS_Vo_Wo(PInst: PInstruction);
begin
  // SQRTPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SQRTPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SQRTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_RSQRTSS_Vo_Wo_d(PInst: PInstruction);
begin
  // RSQRTSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_RSQRTSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RSQRTSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_RSQRTPS_Vo_Wo(PInst: PInstruction);
begin
  // RSQRTPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_RSQRTPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RSQRTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_RCPSS_Vo_Wo_d(PInst: PInstruction);
begin
  // RCPSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_RCPSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCPSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_RCPPS_Vo_Wo(PInst: PInstruction);
begin
  // RCPPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_RCPPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCPPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_ANDPS_Vo_Wo(PInst: PInstruction);
begin
  // ANDPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_LOGICAL or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_ANDPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ANDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_ANDNPS_Vo_Wo(PInst: PInstruction);
begin
  // ANDNPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_LOGICAL or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_ANDNPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ANDNPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_ORPS_Vo_Wo(PInst: PInstruction);
begin
  // ORPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_LOGICAL or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_ORPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ORPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_XORPS_Vo_Wo(PInst: PInstruction);
begin
  // XORPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_LOGICAL or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XORPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XORPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_ADDSS_Vo_Wo_d(PInst: PInstruction);
begin
  // ADDSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_ADDSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_ADDPS_Vo_Wo(PInst: PInstruction);
begin
  // ADDPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_ADDPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MULSS_Vo_Wo_d(PInst: PInstruction);
begin
  // MULSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MULSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MULSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MULPS_Vo_Wo(PInst: PInstruction);
begin
  // MULPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MULPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MULPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_SUBSS_Vo_Wo_d(PInst: PInstruction);
begin
  // SUBSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_SUBSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUBSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_SUBPS_Vo_Wo(PInst: PInstruction);
begin
  // SUBPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SUBPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MINSS_Vo_Wo_d(PInst: PInstruction);
begin
  // MINSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MINSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MINSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MINPS_Vo_Wo(PInst: PInstruction);
begin
  // MINPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MINPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MINPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_DIVSS_Vo_Wo_d(PInst: PInstruction);
begin
  // DIVSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_DIVSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DIVSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_DIVPS_Vo_Wo(PInst: PInstruction);
begin
  // DIVPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_DIVPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DIVPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MAXSS_Vo_Wo_d(PInst: PInstruction);
begin
  // MAXSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SCALAR or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MAXSS;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MAXSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_MAXPS_Vo_Wo(PInst: PInstruction);
begin
  // MAXPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_PACKED or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MAXPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MAXPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_CMPccSS_Vo_Wo_d(PInst: PInstruction);
begin
  // CMPccSS  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  Decode_Vo_Wo_d(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_CMPccSS_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_CMPccSS_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_CMPccPS_Vo_Wo(PInst: PInstruction);
begin
  // CMPccPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.FlagsIndex := $00;
  Decode_Vo_Wo(PInst);
  DecodeImmAsOpExt(PInst);
  PInst^.InstID := INSTS_ID_CMPccPS_Array
    [PInst^.OpImmExt and PInst^.T2N2OpImmMask];
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEMS_CMPccPS_Array[PInst^.OpImmExt and
    PInst^.T2N2OpImmMask]);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_SHUFPS_Vo_Wo_Ib(PInst: PInstruction);
begin
  // SHUFPS  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED or INST_CATEGORY_SHUFFLE or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SHUFPS;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHUFPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_VLDMXCSR_Md(PInst: PInstruction);
begin
  // VLDMXCSR  Md
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VLDMXCSR;
  Decode_Md(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VLDMXCSR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_VSTMXCSR_Md(PInst: PInstruction);
begin
  // VSTMXCSR  Md
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VSTMXCSR;
  Decode_Md(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VSTMXCSR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_PREFETCH_M(PInst: PInstruction);
begin
  // PREFETCH  M
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PREFETCH;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PREFETCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_PREFETCHW_M(PInst: PInstruction);
begin
  // PREFETCHW  M
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PREFETCHW;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PREFETCHW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE_PREFETCHWT1_Mb(PInst: PInstruction);
begin
  // PREFETCHWT1  Mb
  PInst^.InstGroups := INST_GRP_SSE;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PREFETCHWT1;
  Decode_Mb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PREFETCHWT1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
