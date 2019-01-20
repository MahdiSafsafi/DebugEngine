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
// The Original Code is SSE4V1.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SSE4V1_PBLENDVB_Vo_Wo(PInst: PInstruction);
begin
  // PBLENDVB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_BLENDING;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PBLENDVB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PBLENDVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_BLENDVPS_Vo_Wo(PInst: PInstruction);
begin
  // BLENDVPS  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_BLENDING or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_BLENDVPS;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLENDVPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_BLENDVPD_Vo_Wo(PInst: PInstruction);
begin
  // BLENDVPD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_BLENDING or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_BLENDVPD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLENDVPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVSXBW_Vo_Wo_q(PInst: PInstruction);
begin
  // PMOVSXBW  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVSXBW;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVSXBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVSXBD_Vo_Wo_d(PInst: PInstruction);
begin
  // PMOVSXBD  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVSXBD;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVSXBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVSXBQ_Vo_Wo_w(PInst: PInstruction);
begin
  // PMOVSXBQ  Vo,Wo.w
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVSXBQ;
  Decode_Vo_Wo_w(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVSXBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVSXWD_Vo_Wo_q(PInst: PInstruction);
begin
  // PMOVSXWD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVSXWD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVSXWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVSXWQ_Vo_Wo_d(PInst: PInstruction);
begin
  // PMOVSXWQ  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVSXWQ;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVSXWQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVSXDQ_Vo_Wo_q(PInst: PInstruction);
begin
  // PMOVSXDQ  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVSXDQ;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVSXDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMULDQ_Vo_Wo(PInst: PInstruction);
begin
  // PMULDQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMULDQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PCMPEQQ_Vo_Wo(PInst: PInstruction);
begin
  // PCMPEQQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPEQQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPEQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_MOVNTDQA_Vo_Mo(PInst: PInstruction);
begin
  // MOVNTDQA  Vo,Mo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_ALIGNED or INST_CATEGORY_HINT or
    INST_CATEGORY_LOAD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVNTDQA;
  Decode_Vo_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVNTDQA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PACKUSDW_Vo_Wo(PInst: PInstruction);
begin
  // PACKUSDW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_PACK or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PACKUSDW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PACKUSDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVZXBW_Vo_Wo_q(PInst: PInstruction);
begin
  // PMOVZXBW  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVZXBW;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVZXBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVZXBD_Vo_Wo_d(PInst: PInstruction);
begin
  // PMOVZXBD  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVZXBD;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVZXBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVZXBQ_Vo_Wo_w(PInst: PInstruction);
begin
  // PMOVZXBQ  Vo,Wo.w
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVZXBQ;
  Decode_Vo_Wo_w(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVZXBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVZXWD_Vo_Wo_q(PInst: PInstruction);
begin
  // PMOVZXWD  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVZXWD;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVZXWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVZXWQ_Vo_Wo_d(PInst: PInstruction);
begin
  // PMOVZXWQ  Vo,Wo.d
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVZXWQ;
  Decode_Vo_Wo_d(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVZXWQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMOVZXDQ_Vo_Wo_q(PInst: PInstruction);
begin
  // PMOVZXDQ  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMOVZXDQ;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVZXDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMINSB_Vo_Wo(PInst: PInstruction);
begin
  // PMINSB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_MIN or INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMINSB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMINSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMINSD_Vo_Wo(PInst: PInstruction);
begin
  // PMINSD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_MIN or INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMINSD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMINSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMINUW_Vo_Wo(PInst: PInstruction);
begin
  // PMINUW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_MIN or INST_CATEGORY_PACKED or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMINUW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMINUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMINUD_Vo_Wo(PInst: PInstruction);
begin
  // PMINUD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_MIN or INST_CATEGORY_PACKED or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMINUD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMINUD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMAXSB_Vo_Wo(PInst: PInstruction);
begin
  // PMAXSB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_MAX or INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMAXSB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMAXSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMAXSD_Vo_Wo(PInst: PInstruction);
begin
  // PMAXSD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_MAX or INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMAXSD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMAXSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMAXUW_Vo_Wo(PInst: PInstruction);
begin
  // PMAXUW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_MAX or INST_CATEGORY_PACKED or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMAXUW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMAXUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMAXUD_Vo_Wo(PInst: PInstruction);
begin
  // PMAXUD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_MAX or INST_CATEGORY_PACKED or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMAXUD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMAXUD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PMULLD_Vo_Wo(PInst: PInstruction);
begin
  // PMULLD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMULLD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PHMINPOSUW_Vo_Wo(PInst: PInstruction);
begin
  // PHMINPOSUW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_PACKED or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PHMINPOSUW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHMINPOSUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_BLENDPS_Vo_Wo_Ib(PInst: PInstruction);
begin
  // BLENDPS  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_BLENDING or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_BLENDPS;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLENDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_BLENDPD_Vo_Wo_Ib(PInst: PInstruction);
begin
  // BLENDPD  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_BLENDING or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_BLENDPD;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLENDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PBLENDW_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PBLENDW  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_BLENDING;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PBLENDW;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PBLENDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PEXTRB_Mb_Vo_Ib(PInst: PInstruction);
begin
  // PEXTRB  Mb,Vo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_EXTRACTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PEXTRB;
  Decode_Mb_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PEXTRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PEXTRB_Ry_Vo_Ib(PInst: PInstruction);
begin
  // PEXTRB  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_EXTRACTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PEXTRB;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PEXTRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PEXTRW_Mw_Vo_Ib(PInst: PInstruction);
begin
  // PEXTRW  Mw,Vo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_EXTRACTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PEXTRW;
  Decode_Mw_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PEXTRW_Ry_Vo_Ib(PInst: PInstruction);
begin
  // PEXTRW  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_EXTRACTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PEXTRW;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PEXTRQ_Eq_Vo_Ib(PInst: PInstruction);
begin
  // PEXTRQ  Eq,Vo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_EXTRACTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PEXTRQ;
  Decode_Eq_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PEXTRQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PEXTRD_Ed_Vo_Ib(PInst: PInstruction);
begin
  // PEXTRD  Ed,Vo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_EXTRACTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PEXTRD;
  Decode_Ed_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PEXTRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_EXTRACTPS_Md_Vo_Ib(PInst: PInstruction);
begin
  // EXTRACTPS  Md,Vo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_EXTRACTION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_EXTRACTPS;
  Decode_Md_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_EXTRACTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_EXTRACTPS_Ry_Vo_Ib(PInst: PInstruction);
begin
  // EXTRACTPS  Ry,Vo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_EXTRACTION or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_EXTRACTPS;
  Decode_Ry_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_EXTRACTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PINSRB_Vo_Mb_Ib(PInst: PInstruction);
begin
  // PINSRB  Vo,Mb,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_INSERTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PINSRB;
  Decode_Vo_Mb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PINSRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PINSRB_Vo_Rd_Ib(PInst: PInstruction);
begin
  // PINSRB  Vo,Rd,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_INSERTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PINSRB;
  Decode_Vo_Rd_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PINSRB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_INSERTPS_Vo_Md_Ib(PInst: PInstruction);
begin
  // INSERTPS  Vo,Md,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INSERTION or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_INSERTPS;
  Decode_Vo_Md_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INSERTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_INSERTPS_Vo_Uo_Ib(PInst: PInstruction);
begin
  // INSERTPS  Vo,Uo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INSERTION or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_INSERTPS;
  Decode_Vo_Uo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INSERTPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PINSRQ_Vo_Eq_Ib(PInst: PInstruction);
begin
  // PINSRQ  Vo,Eq,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_INSERTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PINSRQ;
  Decode_Vo_Eq_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PINSRQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_PINSRD_Vo_Ed_Ib(PInst: PInstruction);
begin
  // PINSRD  Vo,Ed,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_INSERTION;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PINSRD;
  Decode_Vo_Ed_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PINSRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_DPPS_Vo_Wo_Ib(PInst: PInstruction);
begin
  // DPPS  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_DOT_PRODUCT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_DPPS;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DPPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_DPPD_Vo_Wo_Ib(PInst: PInstruction);
begin
  // DPPD  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := (INST_CATEGORY_DOT_PRODUCT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_DPPD;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DPPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V1_MPSADBW_Vo_Wo_Ib(PInst: PInstruction);
begin
  // MPSADBW  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V1;
  PInst^.InstCategory := INST_CATEGORY_SUMS_ABSOLUTE_DIFFERENCES;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MPSADBW;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MPSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
