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
// The Original Code is MEM-SSE2.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_MEM_SSE2_MOVNTI_My_Gy(PInst: PInstruction);
begin
  // MOVNTI  My,Gy
  PInst^.InstGroups := (INST_GRP_MEM or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVNTI;
  Decode_My_Gy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVNTI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MEM_SSE2_LFENCE_void(PInst: PInstruction);
begin
  // LFENCE  void
  PInst^.InstGroups := (INST_GRP_MEM or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_LOAD);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_LFENCE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LFENCE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MEM_SSE2_MFENCE_void(PInst: PInstruction);
begin
  // MFENCE  void
  PInst^.InstGroups := (INST_GRP_MEM or INST_GRP_SSE2);
  PInst^.InstCategory := (INST_CATEGORY_CACHEABILITY_CONTROL or
    INST_CATEGORY_LOAD or INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MFENCE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MFENCE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
