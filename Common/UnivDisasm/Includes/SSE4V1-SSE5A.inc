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
// The Original Code is SSE4V1-SSE5A.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SSE4V1_SSE5A_PTEST_Vo_Wo(PInst: PInstruction);
begin
  // PTEST  Vo,Wo
  PInst^.InstGroups := (INST_GRP_SSE4V1 or INST_GRP_SSE5A);
  PInst^.InstCategory := INST_CATEGORY_CONDITIONAL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PTEST;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PTEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_SSE5A_ROUNDPS_Vo_Wo_Ib(PInst: PInstruction);
begin
  // ROUNDPS  Vo,Wo,Ib
  PInst^.InstGroups := (INST_GRP_SSE4V1 or INST_GRP_SSE5A);
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED or INST_CATEGORY_ROUND or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ROUNDPS;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROUNDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_SSE5A_ROUNDPD_Vo_Wo_Ib(PInst: PInstruction);
begin
  // ROUNDPD  Vo,Wo,Ib
  PInst^.InstGroups := (INST_GRP_SSE4V1 or INST_GRP_SSE5A);
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_ROUND);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ROUNDPD;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROUNDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_SSE5A_ROUNDSS_Vo_Wo_d_Ib(PInst: PInstruction);
begin
  // ROUNDSS  Vo,Wo.d,Ib
  PInst^.InstGroups := (INST_GRP_SSE4V1 or INST_GRP_SSE5A);
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED or INST_CATEGORY_ROUND or
    INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ROUNDSS;
  Decode_Vo_Wo_d_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROUNDSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_SSE5A_ROUNDSD_Vo_Wo_q_Ib(PInst: PInstruction);
begin
  // ROUNDSD  Vo,Wo.q,Ib
  PInst^.InstGroups := (INST_GRP_SSE4V1 or INST_GRP_SSE5A);
  PInst^.InstCategory := (INST_CATEGORY_DOUBLE_PRECISION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_ROUND);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_ROUNDSD;
  Decode_Vo_Wo_q_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROUNDSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
