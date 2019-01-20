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
// The Original Code is MMX-SSE.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_MMX_SSE_CVTPI2PS_Vo_Nq(PInst: PInstruction);
begin
  // CVTPI2PS  Vo,Nq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CVTPI2PS;
  Decode_Vo_Nq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPI2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_CVTTPS2PI_Pq_Wo_q(PInst: PInstruction);
begin
  // CVTTPS2PI  Pq,Wo.q
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CVTTPS2PI;
  Decode_Pq_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTTPS2PI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_CVTPS2PI_Pq_Wo_q(PInst: PInstruction);
begin
  // CVTPS2PI  Pq,Wo.q
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CVTPS2PI;
  Decode_Pq_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPS2PI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PSHUFW_Pq_Qq_Ib(PInst: PInstruction);
begin
  // PSHUFW  Pq,Qq,Ib
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SHUFFLE or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHUFW;
  Decode_Pq_Qq_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHUFW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PINSRW_Pq_Mw_Ib(PInst: PInstruction);
begin
  // PINSRW  Pq,Mw,Ib
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PINSRW;
  Decode_Pq_Mw_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PINSRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PINSRW_Pq_Rv_Ib(PInst: PInstruction);
begin
  // PINSRW  Pq,Rv,Ib
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PINSRW;
  Decode_Pq_Rv_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PINSRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PEXTRW_Gy_Nq_Ib(PInst: PInstruction);
begin
  // PEXTRW  Gy,Nq,Ib
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := INST_CATEGORY_EXTRACTION;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PEXTRW;
  Decode_Gy_Nq_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PEXTRW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PMOVMSKB_Gy_Nq(PInst: PInstruction);
begin
  // PMOVMSKB  Gy,Nq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMOVMSKB;
  Decode_Gy_Nq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMOVMSKB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PMINUB_Pq_Qq(PInst: PInstruction);
begin
  // PMINUB  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMINUB;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMINUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PMAXUB_Pq_Qq(PInst: PInstruction);
begin
  // PMAXUB  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMAXUB;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMAXUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PAVGB_Pq_Qq(PInst: PInstruction);
begin
  // PAVGB  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PAVGB;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PAVGB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PAVGW_Pq_Qq(PInst: PInstruction);
begin
  // PAVGW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PAVGW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PAVGW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PMULHUW_Pq_Qq(PInst: PInstruction);
begin
  // PMULHUW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_STORE or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMULHUW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULHUW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_MOVNTQ_Mq_Pq(PInst: PInstruction);
begin
  // MOVNTQ  Mq,Pq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVNTQ;
  Decode_Mq_Pq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVNTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PMINSW_Pq_Qq(PInst: PInstruction);
begin
  // PMINSW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMINSW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMINSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PMAXSW_Pq_Qq(PInst: PInstruction);
begin
  // PMAXSW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMAXSW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMAXSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_PSADBW_Pq_Qq(PInst: PInstruction);
begin
  // PSADBW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSADBW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSADBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE_MASKMOVQ_Pq_Nq(PInst: PInstruction);
begin
  // MASKMOVQ  Pq,Nq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MASKMOVQ;
  Decode_Pq_Nq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MASKMOVQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
