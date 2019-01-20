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
// The Original Code is MEM-SSE.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_MEM_SSE_SFENCE_void(PInst: PInstruction);
begin
  // SFENCE  void
  PInst^.InstGroups := (INST_GRP_MEM or INST_GRP_SSE);
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SFENCE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SFENCE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MEM_SSE_PREFETCHNTA_M(PInst: PInstruction);
begin
  // PREFETCHNTA  M
  PInst^.InstGroups := (INST_GRP_MEM or INST_GRP_SSE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PREFETCHNTA;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PREFETCHNTA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MEM_SSE_PREFETCHT0_M(PInst: PInstruction);
begin
  // PREFETCHT0  M
  PInst^.InstGroups := (INST_GRP_MEM or INST_GRP_SSE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PREFETCHT0;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PREFETCHT0);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MEM_SSE_PREFETCHT1_M(PInst: PInstruction);
begin
  // PREFETCHT1  M
  PInst^.InstGroups := (INST_GRP_MEM or INST_GRP_SSE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PREFETCHT1;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PREFETCHT1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MEM_SSE_PREFETCHT2_M(PInst: PInstruction);
begin
  // PREFETCHT2  M
  PInst^.InstGroups := (INST_GRP_MEM or INST_GRP_SSE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PREFETCHT2;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PREFETCHT2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
