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
// The Original Code is MMX-SSSE3.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_MMX_SSSE3_PSHUFB_Pq_Qq(PInst: PInstruction);
begin
  // PSHUFB  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := INST_CATEGORY_SHUFFLE;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSHUFB;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSHUFB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PHADDW_Pq_Qq(PInst: PInstruction);
begin
  // PHADDW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACK or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PHADDD_Pq_Qq(PInst: PInstruction);
begin
  // PHADDD  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACK or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDD;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PHADDSW_Pq_Qq(PInst: PInstruction);
begin
  // PHADDSW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACK or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHADDSW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHADDSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PMADDUBSW_Pq_Qq(PInst: PInstruction);
begin
  // PMADDUBSW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMADDUBSW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMADDUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PHSUBW_Pq_Qq(PInst: PInstruction);
begin
  // PHSUBW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHSUBW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PHSUBD_Pq_Qq(PInst: PInstruction);
begin
  // PHSUBD  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHSUBD;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PHSUBSW_Pq_Qq(PInst: PInstruction);
begin
  // PHSUBSW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_HORIZONTAL or INST_CATEGORY_INTEGER or
    INST_CATEGORY_PACKED or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PHSUBSW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PHSUBSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PSIGNB_Pq_Qq(PInst: PInstruction);
begin
  // PSIGNB  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSIGNB;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSIGNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PSIGNW_Pq_Qq(PInst: PInstruction);
begin
  // PSIGNW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSIGNW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSIGNW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PSIGND_Pq_Qq(PInst: PInstruction);
begin
  // PSIGND  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PSIGND;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PSIGND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PMULHRSW_Pq_Qq(PInst: PInstruction);
begin
  // PMULHRSW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := (INST_CATEGORY_INTEGER or INST_CATEGORY_PACKED or
    INST_CATEGORY_ROUND or INST_CATEGORY_SCALE or INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PMULHRSW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PMULHRSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PABSB_Pq_Qq(PInst: PInstruction);
begin
  // PABSB  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := INST_CATEGORY_SIGNED;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PABSB;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PABSB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PABSW_Pq_Qq(PInst: PInstruction);
begin
  // PABSW  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := INST_CATEGORY_SIGNED;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PABSW;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PABSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PABSD_Pq_Qq(PInst: PInstruction);
begin
  // PABSD  Pq,Qq
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := INST_CATEGORY_SIGNED;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PABSD;
  Decode_Pq_Qq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PABSD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MMX_SSSE3_PALIGNR_Pq_Qq_Ib(PInst: PInstruction);
begin
  // PALIGNR  Pq,Qq,Ib
  PInst^.InstGroups := (INST_GRP_MMX or INST_GRP_SSSE3);
  PInst^.InstCategory := INST_CATEGORY_ALIGN_RIGHT;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PALIGNR;
  Decode_Pq_Qq_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PALIGNR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
