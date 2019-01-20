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
// The Original Code is LWP.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_LWP_LLWPCB_Ry(PInst: PInstruction);
begin
  // LLWPCB  Ry
  PInst^.InstGroups := INST_GRP_LWP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_LLWPCB;
  Decode_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LLWPCB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_LWP_SLWPCB_Ry(PInst: PInstruction);
begin
  // SLWPCB  Ry
  PInst^.InstGroups := INST_GRP_LWP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SLWPCB;
  Decode_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SLWPCB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_LWP_LWPINS_By_Ed_Id(PInst: PInstruction);
begin
  // LWPINS  By,Ed,Id
  PInst^.InstGroups := INST_GRP_LWP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_LWPINS;
  Decode_By_Ed_Id(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LWPINS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_LWP_LWPVAL_By_Ed_Id(PInst: PInstruction);
begin
  // LWPVAL  By,Ed,Id
  PInst^.InstGroups := INST_GRP_LWP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_LWPVAL;
  Decode_By_Ed_Id(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LWPVAL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
