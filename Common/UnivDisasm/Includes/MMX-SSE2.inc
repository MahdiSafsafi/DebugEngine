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
// The Original Code is MMX-SSE2.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_MMX_SSE2_CVTPI2PD_Vo_Nq(PInst: PInstruction);
begin
  // CVTPI2PD  Vo,Nq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CVTPI2PD;
  Decode_Vo_Nq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPI2PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE2_CVTTPD2PI_Pq_Wo(PInst: PInstruction);
begin
  // CVTTPD2PI  Pq,Wo
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CVTTPD2PI;
  Decode_Pq_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTTPD2PI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE2_CVTPD2PI_Pq_Wo(PInst: PInstruction);
begin
  // CVTPD2PI  Pq,Wo
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_DOUBLE_PRECISION or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_PACKED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CVTPD2PI;
  Decode_Pq_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CVTPD2PI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE2_PADDQ_Pq_Qq(PInst: PInstruction);
begin
  // PADDQ  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PADDQ;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PADDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE2_MOVDQ2Q_Pq_Uq(PInst: PInstruction);
begin
  // MOVDQ2Q  Pq,Uq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_MOVDQ2Q;
  Decode_Pq_Uq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVDQ2Q);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE2_MOVQ2DQ_Vo_Nq(PInst: PInstruction);
begin
  // MOVQ2DQ  Vo,Nq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_MOVQ2DQ;
  Decode_Vo_Nq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVQ2DQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE2_PMULUDQ_Pq_Qq(PInst: PInstruction);
begin
  // PMULUDQ  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD or INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMULUDQ;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULUDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSE2_PSUBQ_Pq_Qq(PInst: PInstruction);
begin
  // PSUBQ  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_128_BIT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSUBQ;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSUBQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
