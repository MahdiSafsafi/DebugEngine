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
// The Original Code is MPX.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_MPX_BNDLDX_rB_Mib(PInst: PInstruction);
begin
  // BNDLDX  rB,Mib
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BNDLDX;
  Decode_rB_Mib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDLDX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDMOV_rB_mB(PInst: PInstruction);
begin
  // BNDMOV  rB,mB
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_BNDMOV;
  Decode_rB_mB(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDMOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDMOV_rB_M(PInst: PInstruction);
begin
  // BNDMOV  rB,M
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_BNDMOV;
  Decode_rB_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDMOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDCL_rB_Ey(PInst: PInstruction);
begin
  // BNDCL  rB,Ey
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_BNDCL;
  Decode_rB_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDCL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDCU_rB_Ey(PInst: PInstruction);
begin
  // BNDCU  rB,Ey
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_BNDCU;
  Decode_rB_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDCU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDSTX_Mib_rB(PInst: PInstruction);
begin
  // BNDSTX  Mib,rB
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_BNDSTX;
  Decode_Mib_rB(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDSTX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDMOV_mB_rB(PInst: PInstruction);
begin
  // BNDMOV  mB,rB
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_BNDMOV;
  Decode_mB_rB(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDMOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDMOV_M_rB(PInst: PInstruction);
begin
  // BNDMOV  M,rB
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_BNDMOV;
  Decode_M_rB(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDMOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDMK_rB_My(PInst: PInstruction);
begin
  // BNDMK  rB,My
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_BNDMK;
  Decode_rB_My(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDMK);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_MPX_BNDCN_rB_Ey(PInst: PInstruction);
begin
  // BNDCN  rB,Ey
  PInst^.InstGroups := INST_GRP_MPX;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF2(PInst);
  PInst^.InstID := INST_ID_BNDCN;
  Decode_rB_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BNDCN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
