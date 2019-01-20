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
// The Original Code is BMI.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_BMI_ANDN_Gy_By_Ey(PInst: PInstruction);
begin
  // ANDN  Gy,By,Ey
  PInst^.InstGroups := INST_GRP_BMI;
  PInst^.InstCategory := INST_CATEGORY_BMI1;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_ANDN;
  Decode_Gy_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ANDN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_BMI_BEXTR_Gy_Ey_By(PInst: PInstruction);
begin
  // BEXTR  Gy,Ey,By
  PInst^.InstGroups := INST_GRP_BMI;
  PInst^.InstCategory := INST_CATEGORY_BMI1;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BEXTR;
  Decode_Gy_Ey_By(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BEXTR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_BMI_BLSR_By_Ey(PInst: PInstruction);
begin
  // BLSR  By,Ey
  PInst^.InstGroups := INST_GRP_BMI;
  PInst^.InstCategory := INST_CATEGORY_BMI1;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLSR;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLSR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_BMI_BLSMSK_By_Ey(PInst: PInstruction);
begin
  // BLSMSK  By,Ey
  PInst^.InstGroups := INST_GRP_BMI;
  PInst^.InstCategory := INST_CATEGORY_BMI1;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLSMSK;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLSMSK);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_BMI_BLSI_By_Ey(PInst: PInstruction);
begin
  // BLSI  By,Ey
  PInst^.InstGroups := INST_GRP_BMI;
  PInst^.InstCategory := INST_CATEGORY_BMI1;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLSI;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLSI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
