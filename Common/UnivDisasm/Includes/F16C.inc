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
// The Original Code is F16C.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_F16C_VCVTPH2PS_Vo_Wo_q(PInst: PInstruction);
begin
  // VCVTPH2PS  Vo,Wo.q
  PInst^.InstGroups := INST_GRP_F16C;
  PInst^.InstCategory := (INST_CATEGORY_16_BIT or INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTPH2PS;
  Decode_Vo_Wo_q(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPH2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_F16C_VCVTPH2PS_Vy_Wo_o(PInst: PInstruction);
begin
  // VCVTPH2PS  Vy,Wo.o
  PInst^.InstGroups := INST_GRP_F16C;
  PInst^.InstCategory := (INST_CATEGORY_16_BIT or INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTPH2PS;
  Decode_Vy_Wo_o(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPH2PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_F16C_VCVTPS2PH_Wo_q_Vo_Ib(PInst: PInstruction);
begin
  // VCVTPS2PH  Wo.q,Vo,Ib
  PInst^.InstGroups := INST_GRP_F16C;
  PInst^.InstCategory := (INST_CATEGORY_16_BIT or INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTPS2PH;
  Decode_Wo_q_Vo_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPS2PH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_F16C_VCVTPS2PH_Wo_o_Vy_Ib(PInst: PInstruction);
begin
  // VCVTPS2PH  Wo.o,Vy,Ib
  PInst^.InstGroups := INST_GRP_F16C;
  PInst^.InstCategory := (INST_CATEGORY_16_BIT or INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_SINGLE_PRECISION);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VCVTPS2PH;
  Decode_Wo_o_Vy_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VCVTPS2PH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
