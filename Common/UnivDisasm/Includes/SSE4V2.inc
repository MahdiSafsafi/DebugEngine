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
// The Original Code is SSE4V2.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SSE4V2_PCMPGTQ_Vo_Wo(PInst: PInstruction);
begin
  // PCMPGTQ  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSE4V2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIMD);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPGTQ;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPGTQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V2_CRC32_Gy_Eb(PInst: PInstruction);
begin
  // CRC32  Gy,Eb
  PInst^.InstGroups := INST_GRP_SSE4V2;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_CRC32;
  Decode_Gy_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CRC32);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V2_CRC32_Gy_Ev(PInst: PInstruction);
begin
  // CRC32  Gy,Ev
  PInst^.InstGroups := INST_GRP_SSE4V2;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_CRC32;
  Decode_Gy_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CRC32);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V2_PCMPESTRM_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PCMPESTRM  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_PACKED or
    INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPESTRM;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPESTRM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V2_PCMPESTRI_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PCMPESTRI  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_PACKED or
    INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPESTRI;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPESTRI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V2_PCMPISTRM_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PCMPISTRM  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_PACKED or
    INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPISTRM;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPISTRM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSE4V2_PCMPISTRI_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PCMPISTRI  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSE4V2;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_PACKED or
    INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PCMPISTRI;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PCMPISTRI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
