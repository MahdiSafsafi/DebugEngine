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
// The Original Code is SHA.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SHA_SHA1NEXTE_Vo_Wo(PInst: PInstruction);
begin
  // SHA1NEXTE  Vo,Wo
  PInst^.InstGroups := INST_GRP_SHA;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SHA1NEXTE;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHA1NEXTE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SHA_SHA1MSG1_Vo_Wo(PInst: PInstruction);
begin
  // SHA1MSG1  Vo,Wo
  PInst^.InstGroups := INST_GRP_SHA;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SHA1MSG1;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHA1MSG1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SHA_SHA1MSG2_Vo_Wo(PInst: PInstruction);
begin
  // SHA1MSG2  Vo,Wo
  PInst^.InstGroups := INST_GRP_SHA;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SHA1MSG2;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHA1MSG2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SHA_SHA256RNDS2_Vo_Wo(PInst: PInstruction);
begin
  // SHA256RNDS2  Vo,Wo
  PInst^.InstGroups := INST_GRP_SHA;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SHA256RNDS2;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHA256RNDS2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SHA_SHA256MSG1_Vo_Wo(PInst: PInstruction);
begin
  // SHA256MSG1  Vo,Wo
  PInst^.InstGroups := INST_GRP_SHA;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SHA256MSG1;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHA256MSG1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SHA_SHA256MSG2_Vo_Wo(PInst: PInstruction);
begin
  // SHA256MSG2  Vo,Wo
  PInst^.InstGroups := INST_GRP_SHA;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SHA256MSG2;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHA256MSG2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SHA_SHA1RNDS4_Vo_Wo_Ib(PInst: PInstruction);
begin
  // SHA1RNDS4  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SHA;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SHA1RNDS4;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHA1RNDS4);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
