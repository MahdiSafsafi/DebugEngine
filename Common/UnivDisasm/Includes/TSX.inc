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
// The Original Code is TSX.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_TSX_XABORT_Ib(PInst: PInstruction);
begin
  // XABORT  Ib
  PInst^.InstGroups := INST_GRP_TSX;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XABORT;
  Decode_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XABORT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TSX_XBEGIN_Jz(PInst: PInstruction);
begin
  // XBEGIN  Jz
  PInst^.InstGroups := INST_GRP_TSX;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XBEGIN;
  Decode_Jz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XBEGIN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TSX_XEND_void(PInst: PInstruction);
begin
  // XEND  void
  PInst^.InstGroups := INST_GRP_TSX;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XEND;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XEND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TSX_XTEST_void(PInst: PInstruction);
begin
  // XTEST  void
  PInst^.InstGroups := INST_GRP_TSX;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XTEST;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XTEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
