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
// The Original Code is SSE3.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SSE3_MOVDDUP_Vo_Wo_q(PInst: PInstruction);
begin
  // MOVDDUP  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MOVDDUP;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVDDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_MOVSLDUP_Vo_Wo(PInst: PInstruction);
begin
  // MOVSLDUP  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVSLDUP;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSLDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_MOVSHDUP_Vo_Wo(PInst: PInstruction);
begin
  // MOVSHDUP  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVSHDUP;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSHDUP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_HADDPD_Vo_Wo(PInst: PInstruction);
begin
  // HADDPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_HORIZONTAL or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_HADDPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_HADDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_HADDPS_Vo_Wo(PInst: PInstruction);
begin
  // HADDPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_HORIZONTAL or INST_CATEGORY_SIMD or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_HADDPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_HADDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_HSUBPD_Vo_Wo(PInst: PInstruction);
begin
  // HSUBPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_HORIZONTAL or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_HSUBPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_HSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_HSUBPS_Vo_Wo(PInst: PInstruction);
begin
  // HSUBPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_HORIZONTAL or INST_CATEGORY_SIMD or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_HSUBPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_HSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_ADDSUBPD_Vo_Wo(PInst: PInstruction);
begin
  // ADDSUBPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ADDSUBPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADDSUBPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_ADDSUBPS_Vo_Wo(PInst: PInstruction);
begin
  // ADDSUBPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SIMD or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_ADDSUBPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADDSUBPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_LDDQU_Vo_Mo(PInst: PInstruction);
begin
  // LDDQU  Vo,Mo
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_LOAD or
    INST_CATEGORY_UNALIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_LDDQU;
  Decode_Vo_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LDDQU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_MONITOR_void(PInst: PInstruction);
begin
  // MONITOR  void
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_AGENT_SYNCHRONIZATION or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MONITOR;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MONITOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE3_MWAIT_void(PInst: PInstruction);
begin
  // MWAIT  void
  PInst^.InstGroups := INST_GRP_SSE3;
  PInst^.InstCategory := (INST_CATEGORY_AGENT_SYNCHRONIZATION or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MWAIT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MWAIT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
