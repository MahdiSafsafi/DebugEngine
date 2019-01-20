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
// The Original Code is TBM.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_TBM_BEXTR_Gy_Ey_Id(PInst: PInstruction);
begin
  // BEXTR  Gy,Ey,Id
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.InstCategory := INST_CATEGORY_BMI1;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BEXTR;
  Decode_Gy_Ey_Id(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BEXTR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_BLCFILL_By_Ey(PInst: PInstruction);
begin
  // BLCFILL  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLCFILL;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLCFILL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_BLSFILL_By_Ey(PInst: PInstruction);
begin
  // BLSFILL  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLSFILL;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLSFILL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_BLCS_By_Ey(PInst: PInstruction);
begin
  // BLCS  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLCS;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLCS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_TZMSK_By_Ey(PInst: PInstruction);
begin
  // TZMSK  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_TZMSK;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_TZMSK);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_BLCIC_By_Ey(PInst: PInstruction);
begin
  // BLCIC  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLCIC;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLCIC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_BLSIC_By_Ey(PInst: PInstruction);
begin
  // BLSIC  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLSIC;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLSIC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_T1MSKC_By_Ey(PInst: PInstruction);
begin
  // T1MSKC  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_T1MSKC;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_T1MSKC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_BLCMSK_By_Ey(PInst: PInstruction);
begin
  // BLCMSK  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLCMSK;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLCMSK);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_TBM_BLCI_By_Ey(PInst: PInstruction);
begin
  // BLCI  By,Ey
  PInst^.InstGroups := INST_GRP_TBM;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BLCI;
  Decode_By_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BLCI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
