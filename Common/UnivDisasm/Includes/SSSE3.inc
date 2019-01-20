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
// The Original Code is SSSE3.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SSSE3_PSHUFB_Vo_Wo(PInst: PInstruction);
begin
  // PSHUFB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := INST_CATEGORY_SHUFFLE;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSHUFB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHUFB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PHADDW_Vo_Wo(PInst: PInstruction);
begin
  // PHADDW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACK or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PHADDW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PHADDD_Vo_Wo(PInst: PInstruction);
begin
  // PHADDD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACK or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PHADDD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PHADDSW_Vo_Wo(PInst: PInstruction);
begin
  // PHADDSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACK or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PHADDSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PMADDUBSW_Vo_Wo(PInst: PInstruction);
begin
  // PMADDUBSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMADDUBSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMADDUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PHSUBW_Vo_Wo(PInst: PInstruction);
begin
  // PHSUBW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PHSUBW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PHSUBD_Vo_Wo(PInst: PInstruction);
begin
  // PHSUBD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PHSUBD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PHSUBSW_Vo_Wo(PInst: PInstruction);
begin
  // PHSUBSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PHSUBSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PSIGNB_Vo_Wo(PInst: PInstruction);
begin
  // PSIGNB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSIGNB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSIGNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PSIGNW_Vo_Wo(PInst: PInstruction);
begin
  // PSIGNW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSIGNW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSIGNW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PSIGND_Vo_Wo(PInst: PInstruction);
begin
  // PSIGND  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PSIGND;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSIGND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PMULHRSW_Vo_Wo(PInst: PInstruction);
begin
  // PMULHRSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED or
    INST_CATEGORY_ROUND or INST_CATEGORY_SCALE or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PMULHRSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULHRSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PABSB_Vo_Wo(PInst: PInstruction);
begin
  // PABSB  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := INST_CATEGORY_SIGNED;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PABSB;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PABSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PABSW_Vo_Wo(PInst: PInstruction);
begin
  // PABSW  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := INST_CATEGORY_SIGNED;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PABSW;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PABSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PABSD_Vo_Wo(PInst: PInstruction);
begin
  // PABSD  Vo,Wo
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := INST_CATEGORY_SIGNED;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PABSD;
  Decode_Vo_Wo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PABSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SSSE3_PALIGNR_Vo_Wo_Ib(PInst: PInstruction);
begin
  // PALIGNR  Vo,Wo,Ib
  PInst^.InstGroups := INST_GRP_SSSE3;
  PInst^.InstCategory := INST_CATEGORY_ALIGN_RIGHT;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_PALIGNR;
  Decode_Vo_Wo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PALIGNR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
