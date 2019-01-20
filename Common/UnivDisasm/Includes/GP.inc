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
// The Original Code is GP.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_GP_MOVBE_Gv_Mv(PInst: PInstruction);
begin
  // MOVBE  Gv,Mv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVBE;
  Decode_Gv_Mv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOVBE_Mv_Gv(PInst: PInstruction);
begin
  // MOVBE  Mv,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_MOVBE;
  Decode_Mv_Gv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_Eb_Gb(PInst: PInstruction);
begin
  // ADD  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_Ev_Gv(PInst: PInstruction);
begin
  // ADD  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_Gb_Eb(PInst: PInstruction);
begin
  // ADD  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_Gv_Ev(PInst: PInstruction);
begin
  // ADD  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_AL_Ib(PInst: PInstruction);
begin
  // ADD  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_rAX_Iz(PInst: PInstruction);
begin
  // ADD  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_ES(PInst: PInstruction);
begin
  // PUSH  ES
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.InstID := INST_ID_PUSH;
  Decode_ES(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_ES(PInst: PInstruction);
begin
  // POP  ES
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.InstID := INST_ID_POP;
  Decode_ES(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_Eb_Gb(PInst: PInstruction);
begin
  // OR  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_Ev_Gv(PInst: PInstruction);
begin
  // OR  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_Gb_Eb(PInst: PInstruction);
begin
  // OR  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_Gv_Ev(PInst: PInstruction);
begin
  // OR  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_AL_Ib(PInst: PInstruction);
begin
  // OR  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_rAX_Iz(PInst: PInstruction);
begin
  // OR  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_CS(PInst: PInstruction);
begin
  // PUSH  CS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.InstID := INST_ID_PUSH;
  Decode_CS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_Eb_Gb(PInst: PInstruction);
begin
  // ADC  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_Ev_Gv(PInst: PInstruction);
begin
  // ADC  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_Gb_Eb(PInst: PInstruction);
begin
  // ADC  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_Gv_Ev(PInst: PInstruction);
begin
  // ADC  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_AL_Ib(PInst: PInstruction);
begin
  // ADC  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_rAX_Iz(PInst: PInstruction);
begin
  // ADC  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_SS(PInst: PInstruction);
begin
  // PUSH  SS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.InstID := INST_ID_PUSH;
  Decode_SS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_SS(PInst: PInstruction);
begin
  // POP  SS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.InstID := INST_ID_POP;
  Decode_SS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_Eb_Gb(PInst: PInstruction);
begin
  // SBB  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_Ev_Gv(PInst: PInstruction);
begin
  // SBB  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_Gb_Eb(PInst: PInstruction);
begin
  // SBB  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_Gv_Ev(PInst: PInstruction);
begin
  // SBB  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_AL_Ib(PInst: PInstruction);
begin
  // SBB  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_rAX_Iz(PInst: PInstruction);
begin
  // SBB  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_DS(PInst: PInstruction);
begin
  // PUSH  DS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.InstID := INST_ID_PUSH;
  Decode_DS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_DS(PInst: PInstruction);
begin
  // POP  DS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.InstID := INST_ID_POP;
  Decode_DS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_Eb_Gb(PInst: PInstruction);
begin
  // AND  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_Ev_Gv(PInst: PInstruction);
begin
  // AND  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_Gb_Eb(PInst: PInstruction);
begin
  // AND  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_Gv_Ev(PInst: PInstruction);
begin
  // AND  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_AL_Ib(PInst: PInstruction);
begin
  // AND  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_rAX_Iz(PInst: PInstruction);
begin
  // AND  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DAA_void(PInst: PInstruction);
begin
  // DAA  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_DECIMAL);
  PInst^.FlagsIndex := $1D;
  PInst^.InstID := INST_ID_DAA;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DAA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_Eb_Gb(PInst: PInstruction);
begin
  // SUB  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_Ev_Gv(PInst: PInstruction);
begin
  // SUB  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_Gb_Eb(PInst: PInstruction);
begin
  // SUB  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_Gv_Ev(PInst: PInstruction);
begin
  // SUB  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_AL_Ib(PInst: PInstruction);
begin
  // SUB  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_rAX_Iz(PInst: PInstruction);
begin
  // SUB  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DAS_void(PInst: PInstruction);
begin
  // DAS  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_DECIMAL);
  PInst^.FlagsIndex := $1E;
  PInst^.InstID := INST_ID_DAS;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DAS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_Eb_Gb(PInst: PInstruction);
begin
  // XOR  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_Ev_Gv(PInst: PInstruction);
begin
  // XOR  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_Gb_Eb(PInst: PInstruction);
begin
  // XOR  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_Gv_Ev(PInst: PInstruction);
begin
  // XOR  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_AL_Ib(PInst: PInstruction);
begin
  // XOR  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_rAX_Iz(PInst: PInstruction);
begin
  // XOR  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AAA_void(PInst: PInstruction);
begin
  // AAA  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_DECIMAL);
  PInst^.FlagsIndex := $01;
  PInst^.InstID := INST_ID_AAA;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AAA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_Eb_Gb(PInst: PInstruction);
begin
  // CMP  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_Eb_Gb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_Ev_Gv(PInst: PInstruction);
begin
  // CMP  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_Ev_Gv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_Gb_Eb(PInst: PInstruction);
begin
  // CMP  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_Gv_Ev(PInst: PInstruction);
begin
  // CMP  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_AL_Ib(PInst: PInstruction);
begin
  // CMP  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_rAX_Iz(PInst: PInstruction);
begin
  // CMP  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AAS_void(PInst: PInstruction);
begin
  // AAS  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_DECIMAL);
  PInst^.FlagsIndex := $04;
  PInst^.InstID := INST_ID_AAS;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AAS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_eAX(PInst: PInstruction);
begin
  // INC  eAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_eAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_eCX(PInst: PInstruction);
begin
  // INC  eCX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_eCX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_eDX(PInst: PInstruction);
begin
  // INC  eDX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_eDX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_eBX(PInst: PInstruction);
begin
  // INC  eBX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_eBX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_eSP(PInst: PInstruction);
begin
  // INC  eSP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_eSP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_eBP(PInst: PInstruction);
begin
  // INC  eBP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_eBP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_eSI(PInst: PInstruction);
begin
  // INC  eSI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_eSI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_eDI(PInst: PInstruction);
begin
  // INC  eDI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_eDI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_eAX(PInst: PInstruction);
begin
  // DEC  eAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_eAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_eCX(PInst: PInstruction);
begin
  // DEC  eCX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_eCX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_eDX(PInst: PInstruction);
begin
  // DEC  eDX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_eDX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_eBX(PInst: PInstruction);
begin
  // DEC  eBX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_eBX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_eSP(PInst: PInstruction);
begin
  // DEC  eSP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_eSP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_eBP(PInst: PInstruction);
begin
  // DEC  eBP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_eBP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_eSI(PInst: PInstruction);
begin
  // DEC  eSI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_eSI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_eDI(PInst: PInstruction);
begin
  // DEC  eDI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_eDI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_rAX_D64(PInst: PInstruction);
begin
  // PUSH  rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_r8_D64(PInst: PInstruction);
begin
  // PUSH  r8
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_r8(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_rCX_D64(PInst: PInstruction);
begin
  // PUSH  rCX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_rCX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_r9_D64(PInst: PInstruction);
begin
  // PUSH  r9
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_r9(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_rDX_D64(PInst: PInstruction);
begin
  // PUSH  rDX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_rDX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_r10_D64(PInst: PInstruction);
begin
  // PUSH  r10
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_r10(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_rBX_D64(PInst: PInstruction);
begin
  // PUSH  rBX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_rBX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_r11_D64(PInst: PInstruction);
begin
  // PUSH  r11
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_r11(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_rSP_D64(PInst: PInstruction);
begin
  // PUSH  rSP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_rSP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_r12_D64(PInst: PInstruction);
begin
  // PUSH  r12
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_r12(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_rBP_D64(PInst: PInstruction);
begin
  // PUSH  rBP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_rBP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_r13_D64(PInst: PInstruction);
begin
  // PUSH  r13
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_r13(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_rSI_D64(PInst: PInstruction);
begin
  // PUSH  rSI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_rSI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_r14_D64(PInst: PInstruction);
begin
  // PUSH  r14
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_r14(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_rDI_D64(PInst: PInstruction);
begin
  // PUSH  rDI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_rDI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_r15_D64(PInst: PInstruction);
begin
  // PUSH  r15
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_r15(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_rAX_D64(PInst: PInstruction);
begin
  // POP  rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_r8_D64(PInst: PInstruction);
begin
  // POP  r8
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_r8(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_rCX_D64(PInst: PInstruction);
begin
  // POP  rCX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_rCX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_r9_D64(PInst: PInstruction);
begin
  // POP  r9
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_r9(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_rDX_D64(PInst: PInstruction);
begin
  // POP  rDX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_rDX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_r10_D64(PInst: PInstruction);
begin
  // POP  r10
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_r10(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_rBX_D64(PInst: PInstruction);
begin
  // POP  rBX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_rBX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_r11_D64(PInst: PInstruction);
begin
  // POP  r11
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_r11(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_rSP_D64(PInst: PInstruction);
begin
  // POP  rSP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_rSP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_r12_D64(PInst: PInstruction);
begin
  // POP  r12
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_r12(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_rBP_D64(PInst: PInstruction);
begin
  // POP  rBP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_rBP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_r13_D64(PInst: PInstruction);
begin
  // POP  r13
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_r13(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_rSI_D64(PInst: PInstruction);
begin
  // POP  rSI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_rSI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_r14_D64(PInst: PInstruction);
begin
  // POP  r14
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_r14(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_rDI_D64(PInst: PInstruction);
begin
  // POP  rDI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_rDI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_r15_D64(PInst: PInstruction);
begin
  // POP  r15
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_r15(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSHAD_void(PInst: PInstruction);
begin
  // PUSHAD  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_PUSHAD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSHAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POPAD_void(PInst: PInstruction);
begin
  // POPAD  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_POPAD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POPAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BOUND_Gv_Ma(PInst: PInstruction);
begin
  // BOUND  Gv,Ma
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $09;
  PInst^.InstID := INST_ID_BOUND;
  Decode_Gv_Ma(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BOUND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOVSXD_Gv_Ed(PInst: PInstruction);
begin
  // MOVSXD  Gv,Ed
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_MOVSXD;
  Decode_Gv_Ed(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSXD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_Iz_D64(PInst: PInstruction);
begin
  // PUSH  Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IMUL_Gv_Ev_Iz(PInst: PInstruction);
begin
  // IMUL  Gv,Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $27;
  PInst^.InstID := INST_ID_IMUL;
  Decode_Gv_Ev_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_Ib_D64(PInst: PInstruction);
begin
  // PUSH  Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IMUL_Gv_Ev_Ib(PInst: PInstruction);
begin
  // IMUL  Gv,Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $27;
  PInst^.InstID := INST_ID_IMUL;
  Decode_Gv_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INS_Yb_DX(PInst: PInstruction);
begin
  // INS  Yb,DX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $2A;
  PInst^.InstID := INST_ID_INS;
  Decode_Yb_DX(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INS_Yz_DX(PInst: PInstruction);
begin
  // INS  Yz,DX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $2A;
  PInst^.InstID := INST_ID_INS;
  Decode_Yz_DX(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OUTS_DX_Xb(PInst: PInstruction);
begin
  // OUTS  DX,Xb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $4B;
  PInst^.InstID := INST_ID_OUTS;
  Decode_DX_Xb(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OUTS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OUTS_DX_Xz(PInst: PInstruction);
begin
  // OUTS  DX,Xz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $4B;
  PInst^.InstID := INST_ID_OUTS;
  Decode_DX_Xz(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OUTS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JO_Jb_Df64(PInst: PInstruction);
begin
  // JO  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JO;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JO);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNO_Jb_Df64(PInst: PInstruction);
begin
  // JNO  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNO;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNO);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JB_Jb_Df64(PInst: PInstruction);
begin
  // JB  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JB;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNB_Jb_Df64(PInst: PInstruction);
begin
  // JNB  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNB;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JZ_Jb_Df64(PInst: PInstruction);
begin
  // JZ  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JZ;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JZ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNZ_Jb_Df64(PInst: PInstruction);
begin
  // JNZ  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNZ;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNZ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JBE_Jb_Df64(PInst: PInstruction);
begin
  // JBE  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JBE;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNBE_Jb_Df64(PInst: PInstruction);
begin
  // JNBE  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNBE;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JS_Jb_Df64(PInst: PInstruction);
begin
  // JS  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JS;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNS_Jb_Df64(PInst: PInstruction);
begin
  // JNS  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNS;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JP_Jb_Df64(PInst: PInstruction);
begin
  // JP  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JP;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNP_Jb_Df64(PInst: PInstruction);
begin
  // JNP  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNP;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JL_Jb_Df64(PInst: PInstruction);
begin
  // JL  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JL;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNL_Jb_Df64(PInst: PInstruction);
begin
  // JNL  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNL;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JLE_Jb_Df64(PInst: PInstruction);
begin
  // JLE  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JLE;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JLE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNLE_Jb_Df64(PInst: PInstruction);
begin
  // JNLE  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNLE;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNLE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_TEST_Eb_Gb(PInst: PInstruction);
begin
  // TEST  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $67;
  PInst^.InstID := INST_ID_TEST;
  Decode_Eb_Gb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_TEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_TEST_Ev_Gv(PInst: PInstruction);
begin
  // TEST  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $67;
  PInst^.InstID := INST_ID_TEST;
  Decode_Ev_Gv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_TEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_Eb_Gb(PInst: PInstruction);
begin
  // XCHG  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_Ev_Gv(PInst: PInstruction);
begin
  // XCHG  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Eb_Gb(PInst: PInstruction);
begin
  // MOV  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Eb_Gb(PInst);
  ValidateXRelease(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Ev_Gv(PInst: PInstruction);
begin
  // MOV  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Ev_Gv(PInst);
  ValidateXRelease(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Gb_Eb(PInst: PInstruction);
begin
  // MOV  Gb,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Gb_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Gv_Ev(PInst: PInstruction);
begin
  // MOV  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Mw_Sw(PInst: PInstruction);
begin
  // MOV  Mw,Sw
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Mw_Sw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Rv_Sw(PInst: PInstruction);
begin
  // MOV  Rv,Sw
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Rv_Sw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LEA_Gv_M(PInst: PInstruction);
begin
  // LEA  Gv,M
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_LOAD or INST_CATEGORY_MISCELLANEOUS);
  PInst^.FlagsIndex := $38;
  PInst^.InstID := INST_ID_LEA;
  Decode_Gv_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LEA);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Sw_Mw(PInst: PInstruction);
begin
  // MOV  Sw,Mw
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Sw_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Sw_Rv(PInst: PInstruction);
begin
  // MOV  Sw,Rv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Sw_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_NOP_void(PInst: PInstruction);
begin
  // NOP  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $47;
  PInst^.InstID := INST_ID_NOP;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_NOP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PAUSE_void(PInst: PInstruction);
begin
  // PAUSE  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_PAUSE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PAUSE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_rCX_rAX(PInst: PInstruction);
begin
  // XCHG  rCX,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_rCX_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_r9_rAX(PInst: PInstruction);
begin
  // XCHG  r9,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_r9_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_rDX_rAX(PInst: PInstruction);
begin
  // XCHG  rDX,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_rDX_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_r10_rAX(PInst: PInstruction);
begin
  // XCHG  r10,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_r10_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_rBX_rAX(PInst: PInstruction);
begin
  // XCHG  rBX,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_rBX_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_r11_rAX(PInst: PInstruction);
begin
  // XCHG  r11,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_r11_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_rSP_rAX(PInst: PInstruction);
begin
  // XCHG  rSP,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_rSP_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_r12_rAX(PInst: PInstruction);
begin
  // XCHG  r12,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_r12_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_rBP_rAX(PInst: PInstruction);
begin
  // XCHG  rBP,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_rBP_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_r13_rAX(PInst: PInstruction);
begin
  // XCHG  r13,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_r13_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_rSI_rAX(PInst: PInstruction);
begin
  // XCHG  rSI,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_rSI_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_r14_rAX(PInst: PInstruction);
begin
  // XCHG  r14,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_r14_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_rDI_rAX(PInst: PInstruction);
begin
  // XCHG  rDI,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_rDI_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XCHG_r15_rAX(PInst: PInstruction);
begin
  // XCHG  r15,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $6E;
  PInst^.InstID := INST_ID_XCHG;
  Decode_r15_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CDQE_void(PInst: PInstruction);
begin
  // CDQE  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_CONVERT);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CDQE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CDQE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CBW_void(PInst: PInstruction);
begin
  // CBW  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $0E;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CBW;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CBW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CWDE_void(PInst: PInstruction);
begin
  // CWDE  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CWDE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CWDE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CQO_void(PInst: PInstruction);
begin
  // CQO  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CQO;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CQO);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CWD_void(PInst: PInstruction);
begin
  // CWD  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $1C;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_CWD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CWD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CDQ_void(PInst: PInstruction);
begin
  // CDQ  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CDQ;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CALL_Ap_w_z(PInst: PInstruction);
begin
  // CALL  Ap(w:z)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $0D;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_CALL;
  DecodeBranch_Ap_w_z(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CALL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSHF_void_D64(PInst: PInstruction);
begin
  // PUSHF  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSHF;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSHF);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POPF_void_D64(PInst: PInstruction);
begin
  // POPF  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $4D;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POPF;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POPF);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAHF_void(PInst: PInstruction);
begin
  // SAHF  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_FLAG_CONTROL or INST_CATEGORY_STORE);
  PInst^.FlagsIndex := $59;
  PInst^.InstID := INST_ID_SAHF;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAHF);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LAHF_void(PInst: PInstruction);
begin
  // LAHF  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_FLAG_CONTROL or INST_CATEGORY_LOAD);
  PInst^.FlagsIndex := $35;
  PInst^.InstID := INST_ID_LAHF;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LAHF);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_AL_Ob(PInst: PInstruction);
begin
  // MOV  AL,Ob
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_AL_Ob(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rAX_Ov(PInst: PInstruction);
begin
  // MOV  rAX,Ov
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rAX_Ov(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Ob_AL(PInst: PInstruction);
begin
  // MOV  Ob,AL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Ob_AL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Ov_rAX(PInst: PInstruction);
begin
  // MOV  Ov,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Ov_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOVS_Yb_Xb(PInst: PInstruction);
begin
  // MOVS  Yb,Xb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_STRING;
  PInst^.FlagsIndex := $43;
  PInst^.InstID := INST_ID_MOVS;
  Decode_Yb_Xb(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOVS_Yv_Xv(PInst: PInstruction);
begin
  // MOVS  Yv,Xv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_STRING;
  PInst^.FlagsIndex := $43;
  PInst^.InstID := INST_ID_MOVS;
  Decode_Yv_Xv(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMPS_Yb_Xb(PInst: PInstruction);
begin
  // CMPS  Yb,Xb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $16;
  PInst^.InstID := INST_ID_CMPS;
  Decode_Yb_Xb(PInst);
  ValidateRepe(PInst);
  ValidateRepne(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMPS_Yv_Xv(PInst: PInstruction);
begin
  // CMPS  Yv,Xv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $16;
  PInst^.InstID := INST_ID_CMPS;
  Decode_Yv_Xv(PInst);
  ValidateRepe(PInst);
  ValidateRepne(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_TEST_AL_Ib(PInst: PInstruction);
begin
  // TEST  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $67;
  PInst^.InstID := INST_ID_TEST;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_TEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_TEST_rAX_Iz(PInst: PInstruction);
begin
  // TEST  rAX,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $67;
  PInst^.InstID := INST_ID_TEST;
  Decode_rAX_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_TEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_STOS_Yb_AL(PInst: PInstruction);
begin
  // STOS  Yb,AL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_STORE or INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $64;
  PInst^.InstID := INST_ID_STOS;
  Decode_Yb_AL(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_STOS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_STOS_Yv_rAX(PInst: PInstruction);
begin
  // STOS  Yv,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_STORE or INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $64;
  PInst^.InstID := INST_ID_STOS;
  Decode_Yv_rAX(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_STOS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LODS_AL_Xb(PInst: PInstruction);
begin
  // LODS  AL,Xb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_LOAD or INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $3C;
  PInst^.InstID := INST_ID_LODS;
  Decode_AL_Xb(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LODS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LODS_rAX_Xv(PInst: PInstruction);
begin
  // LODS  rAX,Xv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_LOAD or INST_CATEGORY_STRING);
  PInst^.FlagsIndex := $3C;
  PInst^.InstID := INST_ID_LODS;
  Decode_rAX_Xv(PInst);
  ValidateRep(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LODS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SCAS_Yb_AL(PInst: PInstruction);
begin
  // SCAS  Yb,AL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_STRING;
  PInst^.FlagsIndex := $5D;
  PInst^.InstID := INST_ID_SCAS;
  Decode_Yb_AL(PInst);
  ValidateRepe(PInst);
  ValidateRepne(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SCAS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SCAS_Yv_rAX(PInst: PInstruction);
begin
  // SCAS  Yv,rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_STRING;
  PInst^.FlagsIndex := $5D;
  PInst^.InstID := INST_ID_SCAS;
  Decode_Yv_rAX(PInst);
  ValidateRepe(PInst);
  ValidateRepne(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SCAS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_AL_Ib(PInst: PInstruction);
begin
  // MOV  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_R8B_Ib(PInst: PInstruction);
begin
  // MOV  R8B,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_R8B_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_CL_Ib(PInst: PInstruction);
begin
  // MOV  CL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.InstID := INST_ID_MOV;
  Decode_CL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_R9B_Ib(PInst: PInstruction);
begin
  // MOV  R9B,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_R9B_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_DL_Ib(PInst: PInstruction);
begin
  // MOV  DL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.InstID := INST_ID_MOV;
  Decode_DL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_R10B_Ib(PInst: PInstruction);
begin
  // MOV  R10B,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_R10B_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_BL_Ib(PInst: PInstruction);
begin
  // MOV  BL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_BL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_R11B_Ib(PInst: PInstruction);
begin
  // MOV  R11B,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_R11B_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_AH_Ib(PInst: PInstruction);
begin
  // MOV  AH,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_AH_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_R12B_Ib(PInst: PInstruction);
begin
  // MOV  R12B,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_R12B_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_CH_Ib(PInst: PInstruction);
begin
  // MOV  CH,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.InstID := INST_ID_MOV;
  Decode_CH_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_R13B_Ib(PInst: PInstruction);
begin
  // MOV  R13B,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_R13B_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_DH_Ib(PInst: PInstruction);
begin
  // MOV  DH,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.InstID := INST_ID_MOV;
  Decode_DH_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_R14B_Ib(PInst: PInstruction);
begin
  // MOV  R14B,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_R14B_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_BH_Ib(PInst: PInstruction);
begin
  // MOV  BH,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_BH_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_R15B_Ib(PInst: PInstruction);
begin
  // MOV  R15B,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_R15B_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rAX_Iv(PInst: PInstruction);
begin
  // MOV  rAX,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rAX_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_r8_Iv(PInst: PInstruction);
begin
  // MOV  r8,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_r8_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rCX_Iv(PInst: PInstruction);
begin
  // MOV  rCX,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rCX_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_r9_Iv(PInst: PInstruction);
begin
  // MOV  r9,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_r9_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rDX_Iv(PInst: PInstruction);
begin
  // MOV  rDX,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rDX_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_r10_Iv(PInst: PInstruction);
begin
  // MOV  r10,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_r10_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rBX_Iv(PInst: PInstruction);
begin
  // MOV  rBX,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rBX_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_r11_Iv(PInst: PInstruction);
begin
  // MOV  r11,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_r11_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rSP_Iv(PInst: PInstruction);
begin
  // MOV  rSP,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rSP_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_r12_Iv(PInst: PInstruction);
begin
  // MOV  r12,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_r12_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rBP_Iv(PInst: PInstruction);
begin
  // MOV  rBP,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rBP_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_r13_Iv(PInst: PInstruction);
begin
  // MOV  r13,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_r13_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rSI_Iv(PInst: PInstruction);
begin
  // MOV  rSI,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rSI_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_r14_Iv(PInst: PInstruction);
begin
  // MOV  r14,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_r14_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_rDI_Iv(PInst: PInstruction);
begin
  // MOV  rDI,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_rDI_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_r15_Iv(PInst: PInstruction);
begin
  // MOV  r15,Iv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_r15_Iv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RET_Iw_Df64(PInst: PInstruction);
begin
  // RET  Iw
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $55;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_NEAR;
  PInst^.InstID := INST_ID_RET;
  DecodeBranch_Iw(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RET);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RET_void_Df64(PInst: PInstruction);
begin
  // RET  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $55;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_NEAR;
  PInst^.InstID := INST_ID_RET;
  DecodeBranch_void(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RET);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LES_Gv_Mp_w_v(PInst: PInstruction);
begin
  // LES  Gv,Mp(w:v)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_LOAD or INST_CATEGORY_SEGMENT_REGISTER);
  PInst^.FlagsIndex := $37;
  PInst^.InstID := INST_ID_LES;
  Decode_Gv_Mp_w_v(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LES);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LDS_Gv_Mp_w_v(PInst: PInstruction);
begin
  // LDS  Gv,Mp(w:v)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_LOAD or INST_CATEGORY_SEGMENT_REGISTER);
  PInst^.FlagsIndex := $37;
  PInst^.InstID := INST_ID_LDS;
  Decode_Gv_Mp_w_v(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LDS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ENTER_Iw_Ib_D64(PInst: PInstruction);
begin
  // ENTER  Iw,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ENTER_LEAVE;
  PInst^.FlagsIndex := $21;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_ENTER;
  Decode_Iw_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ENTER);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LEAVE_void_D64(PInst: PInstruction);
begin
  // LEAVE  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ENTER_LEAVE;
  PInst^.FlagsIndex := $39;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_LEAVE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LEAVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RET_Iw(PInst: PInstruction);
begin
  // RET  Iw
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $55;
  PInst^.DstAddr.Flags := JF_BRANCH or JF_FAR;
  PInst^.InstID := INST_ID_RET;
  DecodeBranch_Iw(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RET);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RET_void(PInst: PInstruction);
begin
  // RET  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $55;
  PInst^.DstAddr.Flags := JF_BRANCH or JF_FAR;
  PInst^.InstID := INST_ID_RET;
  DecodeBranch_void(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RET);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INT3_void(PInst: PInstruction);
begin
  // INT3  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_INT3;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INT3);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INT_Ib(PInst: PInstruction);
begin
  // INT  Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $2B;
  PInst^.InstID := INST_ID_INT;
  Decode_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INTO_void(PInst: PInstruction);
begin
  // INTO  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $2C;
  PInst^.InstID := INST_ID_INTO;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INTO);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IRET_void(PInst: PInstruction);
begin
  // IRET  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $31;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_IRET;
  DecodeBranch_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IRET);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AAM_Ib(PInst: PInstruction);
begin
  // AAM  Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_DECIMAL);
  PInst^.FlagsIndex := $03;
  PInst^.InstID := INST_ID_AAM;
  Decode_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AAM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AAD_Ib(PInst: PInstruction);
begin
  // AAD  Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_DECIMAL);
  PInst^.FlagsIndex := $02;
  PInst^.InstID := INST_ID_AAD;
  Decode_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETALC_void(PInst: PInstruction);
begin
  // SETALC  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETALC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETALC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XLAT_void(PInst: PInstruction);
begin
  // XLAT  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $6F;
  PInst^.InstID := INST_ID_XLAT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XLAT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LOOPNE_Jb_Df64(PInst: PInstruction);
begin
  // LOOPNE  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $3E;
  PInst^.SetSizeToDf64();
  PInst^.InstID := INST_ID_LOOPNE;
  Decode_Jb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LOOPNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LOOPE_Jb_Df64(PInst: PInstruction);
begin
  // LOOPE  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $3E;
  PInst^.SetSizeToDf64();
  PInst^.InstID := INST_ID_LOOPE;
  Decode_Jb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LOOPE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LOOP_Jb_Df64(PInst: PInstruction);
begin
  // LOOP  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $3D;
  PInst^.SetSizeToDf64();
  PInst^.InstID := INST_ID_LOOP;
  Decode_Jb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LOOP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JRCX_Jb_Df64(PInst: PInstruction);
begin
  // JRCX  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JRCX;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JRCX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JCXZ_Jb_Df64(PInst: PInstruction);
begin
  // JCXZ  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JCXZ;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JCXZ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JECX_Jb_Df64(PInst: PInstruction);
begin
  // JECX  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JECX;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JECX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IN_AL_Ib(PInst: PInstruction);
begin
  // IN  AL,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $28;
  PInst^.InstID := INST_ID_IN;
  Decode_AL_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IN_eAX_Ib(PInst: PInstruction);
begin
  // IN  eAX,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $28;
  PInst^.InstID := INST_ID_IN;
  Decode_eAX_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OUT_Ib_AL(PInst: PInstruction);
begin
  // OUT  Ib,AL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $4A;
  PInst^.InstID := INST_ID_OUT;
  Decode_Ib_AL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OUT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OUT_Ib_eAX(PInst: PInstruction);
begin
  // OUT  Ib,eAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $4A;
  PInst^.InstID := INST_ID_OUT;
  Decode_Ib_eAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OUT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CALL_Jz_Df64(PInst: PInstruction);
begin
  // CALL  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $0D;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_CALL;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CALL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JMP_Jz_Df64(PInst: PInstruction);
begin
  // JMP  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $34;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JMP;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JMP_Ap_w_z(PInst: PInstruction);
begin
  // JMP  Ap(w:z)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $34;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JMP;
  DecodeBranch_Ap_w_z(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JMP_Jb_Df64(PInst: PInstruction);
begin
  // JMP  Jb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $34;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JMP;
  Decode_Jb(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IN_AL_DX(PInst: PInstruction);
begin
  // IN  AL,DX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $28;
  PInst^.InstID := INST_ID_IN;
  Decode_AL_DX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IN_eAX_DX(PInst: PInstruction);
begin
  // IN  eAX,DX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $28;
  PInst^.InstID := INST_ID_IN;
  Decode_eAX_DX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OUT_DX_AL(PInst: PInstruction);
begin
  // OUT  DX,AL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $4A;
  PInst^.InstID := INST_ID_OUT;
  Decode_DX_AL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OUT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OUT_DX_eAX(PInst: PInstruction);
begin
  // OUT  DX,eAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_IO;
  PInst^.FlagsIndex := $4A;
  PInst^.InstID := INST_ID_OUT;
  Decode_DX_eAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OUT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMC_void(PInst: PInstruction);
begin
  // CMC  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $13;
  PInst^.InstID := INST_ID_CMC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CLC_void(PInst: PInstruction);
begin
  // CLC  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $0F;
  PInst^.InstID := INST_ID_CLC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CLC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_STC_void(PInst: PInstruction);
begin
  // STC  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $61;
  PInst^.InstID := INST_ID_STC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_STC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CLI_void(PInst: PInstruction);
begin
  // CLI  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $11;
  PInst^.InstID := INST_ID_CLI;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CLI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_STI_void(PInst: PInstruction);
begin
  // STI  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $63;
  PInst^.InstID := INST_ID_STI;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_STI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CLD_void(PInst: PInstruction);
begin
  // CLD  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $10;
  PInst^.InstID := INST_ID_CLD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_STD_void(PInst: PInstruction);
begin
  // STD  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_FLAG_CONTROL;
  PInst^.FlagsIndex := $62;
  PInst^.InstID := INST_ID_STD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_STD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SYSCALL_void(PInst: PInstruction);
begin
  // SYSCALL  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_64_BIT;
  PInst^.FlagsIndex := $00;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_SYSCALL;
  DecodeBranch_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SYSCALL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SYSRET_void(PInst: PInstruction);
begin
  // SYSRET  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_64_BIT;
  PInst^.FlagsIndex := $00;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_SYSRET;
  DecodeBranch_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SYSRET);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Rd_CR8D(PInst: PInstruction);
begin
  // MOV  Rd,CR8D
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  MakeMndPrefixF0(PInst);
  PInst^.InstID := INST_ID_MOV;
  Decode_Rd_CR8D(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Ry_Cy_F64(PInst: PInstruction);
begin
  // MOV  Ry,Cy
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_MOV;
  Decode_Ry_Cy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Ry_Dy_F64(PInst: PInstruction);
begin
  // MOV  Ry,Dy
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_MOV;
  Decode_Ry_Dy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_CR8D_Rd(PInst: PInstruction);
begin
  // MOV  CR8D,Rd
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  MakeMndPrefixF0(PInst);
  PInst^.InstID := INST_ID_MOV;
  Decode_CR8D_Rd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Cy_Ry_F64(PInst: PInstruction);
begin
  // MOV  Cy,Ry
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_MOV;
  Decode_Cy_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Dy_Ry_F64(PInst: PInstruction);
begin
  // MOV  Dy,Ry
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_MOV;
  Decode_Dy_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Ry_Ty_F64(PInst: PInstruction);
begin
  // MOV  Ry,Ty
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_MOV;
  Decode_Ry_Ty(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Ty_Ry_F64(PInst: PInstruction);
begin
  // MOV  Ty,Ry
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $41;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_MOV;
  Decode_Ty_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JO_Jz_Df64(PInst: PInstruction);
begin
  // JO  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JO;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JO);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNO_Jz_Df64(PInst: PInstruction);
begin
  // JNO  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNO;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNO);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JB_Jz_Df64(PInst: PInstruction);
begin
  // JB  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JB;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNB_Jz_Df64(PInst: PInstruction);
begin
  // JNB  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNB;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JZ_Jz_Df64(PInst: PInstruction);
begin
  // JZ  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JZ;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JZ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNZ_Jz_Df64(PInst: PInstruction);
begin
  // JNZ  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNZ;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNZ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JBE_Jz_Df64(PInst: PInstruction);
begin
  // JBE  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JBE;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNBE_Jz_Df64(PInst: PInstruction);
begin
  // JNBE  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNBE;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JS_Jz_Df64(PInst: PInstruction);
begin
  // JS  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JS;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNS_Jz_Df64(PInst: PInstruction);
begin
  // JNS  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNS;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JP_Jz_Df64(PInst: PInstruction);
begin
  // JP  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JP;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNP_Jz_Df64(PInst: PInstruction);
begin
  // JNP  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNP;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JL_Jz_Df64(PInst: PInstruction);
begin
  // JL  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JL;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNL_Jz_Df64(PInst: PInstruction);
begin
  // JNL  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNL;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JLE_Jz_Df64(PInst: PInstruction);
begin
  // JLE  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JLE;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JLE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JNLE_Jz_Df64(PInst: PInstruction);
begin
  // JNLE  Jz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $32;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH or JF_JCC;
  PInst^.InstID := INST_ID_JNLE;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JNLE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETO_Eb(PInst: PInstruction);
begin
  // SETO  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETO;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETO);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETNO_Eb(PInst: PInstruction);
begin
  // SETNO  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETNO;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETNO);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETB_Eb(PInst: PInstruction);
begin
  // SETB  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETB;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETNB_Eb(PInst: PInstruction);
begin
  // SETNB  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETNB;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETZ_Eb(PInst: PInstruction);
begin
  // SETZ  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETZ;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETZ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETNZ_Eb(PInst: PInstruction);
begin
  // SETNZ  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETNZ;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETNZ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETBE_Eb(PInst: PInstruction);
begin
  // SETBE  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETBE;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETNBE_Eb(PInst: PInstruction);
begin
  // SETNBE  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETNBE;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETS_Eb(PInst: PInstruction);
begin
  // SETS  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETS;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETNS_Eb(PInst: PInstruction);
begin
  // SETNS  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETNS;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETNS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETP_Eb(PInst: PInstruction);
begin
  // SETP  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETP;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETNP_Eb(PInst: PInstruction);
begin
  // SETNP  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETNP;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETNP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETL_Eb(PInst: PInstruction);
begin
  // SETL  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETL;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETNL_Eb(PInst: PInstruction);
begin
  // SETNL  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETNL;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETNL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETLE_Eb(PInst: PInstruction);
begin
  // SETLE  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETLE;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETLE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SETNLE_Eb(PInst: PInstruction);
begin
  // SETNLE  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_CONDITIONAL);
  PInst^.FlagsIndex := $5E;
  PInst^.InstID := INST_ID_SETNLE;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SETNLE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_FS_D64(PInst: PInstruction);
begin
  // PUSH  FS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_FS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_FS_D64(PInst: PInstruction);
begin
  // POP  FS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_FS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CPUID_void(PInst: PInstruction);
begin
  // CPUID  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $1B;
  PInst^.InstID := INST_ID_CPUID;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CPUID);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BT_Ev_Gv(PInst: PInstruction);
begin
  // BT  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0C;
  PInst^.InstID := INST_ID_BT;
  Decode_Ev_Gv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHLD_Ev_Gv_Ib(PInst: PInstruction);
begin
  // SHLD  Ev,Gv,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $60;
  PInst^.InstID := INST_ID_SHLD;
  Decode_Ev_Gv_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHLD_Ev_Gv_CL(PInst: PInstruction);
begin
  // SHLD  Ev,Gv,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $60;
  PInst^.InstID := INST_ID_SHLD;
  Decode_Ev_Gv_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMPXCHG_void(PInst: PInstruction);
begin
  // CMPXCHG  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $17;
  PInst^.InstID := INST_ID_CMPXCHG;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMPXCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_GS_D64(PInst: PInstruction);
begin
  // PUSH  GS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_GS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_GS_D64(PInst: PInstruction);
begin
  // POP  GS
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_GS(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BTS_Ev_Gv(PInst: PInstruction);
begin
  // BTS  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0C;
  PInst^.InstID := INST_ID_BTS;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BTS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHRD_Ev_Gv_Ib(PInst: PInstruction);
begin
  // SHRD  Ev,Gv,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $60;
  PInst^.InstID := INST_ID_SHRD;
  Decode_Ev_Gv_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHRD_Ev_Gv_CL(PInst: PInstruction);
begin
  // SHRD  Ev,Gv,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $60;
  PInst^.InstID := INST_ID_SHRD;
  Decode_Ev_Gv_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHRD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IMUL_Gv_Ev(PInst: PInstruction);
begin
  // IMUL  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $27;
  PInst^.InstID := INST_ID_IMUL;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMPXCHG_Eb_Gb(PInst: PInstruction);
begin
  // CMPXCHG  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $17;
  PInst^.InstID := INST_ID_CMPXCHG;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMPXCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMPXCHG_Ev_Gv(PInst: PInstruction);
begin
  // CMPXCHG  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $17;
  PInst^.InstID := INST_ID_CMPXCHG;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMPXCHG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LSS_Gv_Mp_w_v(PInst: PInstruction);
begin
  // LSS  Gv,Mp(w:v)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_LOAD or INST_CATEGORY_SEGMENT_REGISTER);
  PInst^.FlagsIndex := $37;
  PInst^.InstID := INST_ID_LSS;
  Decode_Gv_Mp_w_v(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LSS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BTR_Ev_Gv(PInst: PInstruction);
begin
  // BTR  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0C;
  PInst^.InstID := INST_ID_BTR;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BTR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LFS_Gv_Mp_w_v(PInst: PInstruction);
begin
  // LFS  Gv,Mp(w:v)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_LOAD or INST_CATEGORY_SEGMENT_REGISTER);
  PInst^.FlagsIndex := $37;
  PInst^.InstID := INST_ID_LFS;
  Decode_Gv_Mp_w_v(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LFS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_LGS_Gv_Mp_w_v(PInst: PInstruction);
begin
  // LGS  Gv,Mp(w:v)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_LOAD or INST_CATEGORY_SEGMENT_REGISTER);
  PInst^.FlagsIndex := $37;
  PInst^.InstID := INST_ID_LGS;
  Decode_Gv_Mp_w_v(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LGS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOVZX_Gv_Eb(PInst: PInstruction);
begin
  // MOVZX  Gv,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_64_BIT;
  PInst^.FlagsIndex := $44;
  PInst^.InstID := INST_ID_MOVZX;
  Decode_Gv_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVZX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOVZX_Gv_Ew(PInst: PInstruction);
begin
  // MOVZX  Gv,Ew
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_64_BIT;
  PInst^.FlagsIndex := $44;
  PInst^.InstID := INST_ID_MOVZX;
  Decode_Gv_Ew(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVZX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POPCNT_Gv_Ev(PInst: PInstruction);
begin
  // POPCNT  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_POPCNT;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POPCNT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_UD2_void(PInst: PInstruction);
begin
  // UD2  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $68;
  PInst^.InstID := INST_ID_UD2;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_UD2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BTC_Ev_Gv(PInst: PInstruction);
begin
  // BTC  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0C;
  PInst^.InstID := INST_ID_BTC;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BTC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSF_Gv_Ev(PInst: PInstruction);
begin
  // BSF  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0A;
  PInst^.InstID := INST_ID_BSF;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSF);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSR_Gv_Ev(PInst: PInstruction);
begin
  // BSR  Gv,Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0A;
  PInst^.InstID := INST_ID_BSR;
  Decode_Gv_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOVSX_Gv_Eb(PInst: PInstruction);
begin
  // MOVSX  Gv,Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $44;
  PInst^.InstID := INST_ID_MOVSX;
  Decode_Gv_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOVSX_Gv_Ew(PInst: PInstruction);
begin
  // MOVSX  Gv,Ew
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $44;
  PInst^.InstID := INST_ID_MOVSX;
  Decode_Gv_Ew(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOVSX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XADD_Eb_Gb(PInst: PInstruction);
begin
  // XADD  Eb,Gb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $6D;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_XADD;
  Decode_Eb_Gb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XADD_Ev_Gv(PInst: PInstruction);
begin
  // XADD  Ev,Gv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $6D;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_XADD;
  Decode_Ev_Gv(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_rAX(PInst: PInstruction);
begin
  // BSWAP  rAX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_rAX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_r8(PInst: PInstruction);
begin
  // BSWAP  r8
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_r8(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_rCX(PInst: PInstruction);
begin
  // BSWAP  rCX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_rCX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_r9(PInst: PInstruction);
begin
  // BSWAP  r9
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_r9(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_rDX(PInst: PInstruction);
begin
  // BSWAP  rDX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_rDX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_r10(PInst: PInstruction);
begin
  // BSWAP  r10
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_r10(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_rBX(PInst: PInstruction);
begin
  // BSWAP  rBX
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_rBX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_r11(PInst: PInstruction);
begin
  // BSWAP  r11
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_r11(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_rSP(PInst: PInstruction);
begin
  // BSWAP  rSP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_rSP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_r12(PInst: PInstruction);
begin
  // BSWAP  r12
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_r12(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_rBP(PInst: PInstruction);
begin
  // BSWAP  rBP
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_rBP(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_r13(PInst: PInstruction);
begin
  // BSWAP  r13
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_r13(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_rSI(PInst: PInstruction);
begin
  // BSWAP  rSI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_rSI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_r14(PInst: PInstruction);
begin
  // BSWAP  r14
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_r14(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_rDI(PInst: PInstruction);
begin
  // BSWAP  rDI
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_rDI(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BSWAP_r15(PInst: PInstruction);
begin
  // BSWAP  r15
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $0B;
  PInst^.InstID := INST_ID_BSWAP;
  Decode_r15(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BSWAP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_UD_void(PInst: PInstruction);
begin
  // UD  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_UD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_UD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_Eb_Ib(PInst: PInstruction);
begin
  // ADD  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_Eb_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_Eb_Ib(PInst: PInstruction);
begin
  // OR  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_Eb_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_Eb_Ib(PInst: PInstruction);
begin
  // ADC  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_Eb_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_Eb_Ib(PInst: PInstruction);
begin
  // SBB  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_Eb_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_Eb_Ib(PInst: PInstruction);
begin
  // AND  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_Eb_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_Eb_Ib(PInst: PInstruction);
begin
  // SUB  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_Eb_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_Eb_Ib(PInst: PInstruction);
begin
  // XOR  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_Eb_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_Eb_Ib(PInst: PInstruction);
begin
  // CMP  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_Ev_Iz(PInst: PInstruction);
begin
  // ADD  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_Ev_Iz(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_Ev_Iz(PInst: PInstruction);
begin
  // OR  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_Ev_Iz(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_Ev_Iz(PInst: PInstruction);
begin
  // ADC  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_Ev_Iz(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_Ev_Iz(PInst: PInstruction);
begin
  // SBB  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_Ev_Iz(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_Ev_Iz(PInst: PInstruction);
begin
  // AND  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_Ev_Iz(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_Ev_Iz(PInst: PInstruction);
begin
  // SUB  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_Ev_Iz(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_Ev_Iz(PInst: PInstruction);
begin
  // XOR  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_Ev_Iz(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_Ev_Iz(PInst: PInstruction);
begin
  // CMP  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_Ev_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADD_Ev_Ib(PInst: PInstruction);
begin
  // ADD  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_INTEGER);
  PInst^.FlagsIndex := $06;
  PInst^.InstID := INST_ID_ADD;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_OR_Ev_Ib(PInst: PInstruction);
begin
  // OR  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $49;
  PInst^.InstID := INST_ID_OR;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_OR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ADC_Ev_Ib(PInst: PInstruction);
begin
  // ADC  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $05;
  PInst^.InstID := INST_ID_ADC;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ADC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SBB_Ev_Ib(PInst: PInstruction);
begin
  // SBB  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $5C;
  PInst^.InstID := INST_ID_SBB;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SBB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_AND_Ev_Ib(PInst: PInstruction);
begin
  // AND  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $07;
  PInst^.InstID := INST_ID_AND;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_AND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SUB_Ev_Ib(PInst: PInstruction);
begin
  // SUB  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $66;
  PInst^.InstID := INST_ID_SUB;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_XOR_Ev_Ib(PInst: PInstruction);
begin
  // XOR  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $70;
  PInst^.InstID := INST_ID_XOR;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMP_Ev_Ib(PInst: PInstruction);
begin
  // CMP  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $15;
  PInst^.InstID := INST_ID_CMP;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Eb_Ib(PInst: PInstruction);
begin
  // MOV  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Eb_Ib(PInst);
  ValidateXRelease(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MOV_Ev_Iz(PInst: PInstruction);
begin
  // MOV  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $42;
  PInst^.InstID := INST_ID_MOV;
  Decode_Ev_Iz(PInst);
  ValidateXRelease(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MOV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_NOP_Ev(PInst: PInstruction);
begin
  // NOP  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $47;
  PInst^.InstID := INST_ID_NOP;
  Decode_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_NOP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_POP_Ev_D64(PInst: PInstruction);
begin
  // POP  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4C;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_POP;
  Decode_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_POP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROL_Eb_Ib(PInst: PInstruction);
begin
  // ROL  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $57;
  PInst^.InstID := INST_ID_ROL;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROR_Eb_Ib(PInst: PInstruction);
begin
  // ROR  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $57;
  PInst^.InstID := INST_ID_ROR;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCL_Eb_Ib(PInst: PInstruction);
begin
  // RCL  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $50;
  PInst^.InstID := INST_ID_RCL;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCR_Eb_Ib(PInst: PInstruction);
begin
  // RCR  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $50;
  PInst^.InstID := INST_ID_RCR;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHL_Eb_Ib(PInst: PInstruction);
begin
  // SHL  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SHL;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHR_Eb_Ib(PInst: PInstruction);
begin
  // SHR  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SHR;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAL_Eb_Ib(PInst: PInstruction);
begin
  // SAL  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SAL;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAR_Eb_Ib(PInst: PInstruction);
begin
  // SAR  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SAR;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROL_Ev_Ib(PInst: PInstruction);
begin
  // ROL  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $57;
  PInst^.InstID := INST_ID_ROL;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROR_Ev_Ib(PInst: PInstruction);
begin
  // ROR  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $57;
  PInst^.InstID := INST_ID_ROR;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCL_Ev_Ib(PInst: PInstruction);
begin
  // RCL  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $50;
  PInst^.InstID := INST_ID_RCL;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCR_Ev_Ib(PInst: PInstruction);
begin
  // RCR  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $50;
  PInst^.InstID := INST_ID_RCR;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHL_Ev_Ib(PInst: PInstruction);
begin
  // SHL  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SHL;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHR_Ev_Ib(PInst: PInstruction);
begin
  // SHR  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SHR;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAL_Ev_Ib(PInst: PInstruction);
begin
  // SAL  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SAL;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAR_Ev_Ib(PInst: PInstruction);
begin
  // SAR  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SAR;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROL_Eb_1(PInst: PInstruction);
begin
  // ROL  Eb,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $56;
  PInst^.InstID := INST_ID_ROL;
  Decode_Eb_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROR_Eb_1(PInst: PInstruction);
begin
  // ROR  Eb,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $56;
  PInst^.InstID := INST_ID_ROR;
  Decode_Eb_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCL_Eb_1(PInst: PInstruction);
begin
  // RCL  Eb,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $4F;
  PInst^.InstID := INST_ID_RCL;
  Decode_Eb_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCR_Eb_1(PInst: PInstruction);
begin
  // RCR  Eb,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $4F;
  PInst^.InstID := INST_ID_RCR;
  Decode_Eb_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHL_Eb_1(PInst: PInstruction);
begin
  // SHL  Eb,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5A;
  PInst^.InstID := INST_ID_SHL;
  Decode_Eb_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHR_Eb_1(PInst: PInstruction);
begin
  // SHR  Eb,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5A;
  PInst^.InstID := INST_ID_SHR;
  Decode_Eb_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAL_Eb_1(PInst: PInstruction);
begin
  // SAL  Eb,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5A;
  PInst^.InstID := INST_ID_SAL;
  Decode_Eb_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAR_Eb_1(PInst: PInstruction);
begin
  // SAR  Eb,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5A;
  PInst^.InstID := INST_ID_SAR;
  Decode_Eb_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROL_Ev_1(PInst: PInstruction);
begin
  // ROL  Ev,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $56;
  PInst^.InstID := INST_ID_ROL;
  Decode_Ev_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROR_Ev_1(PInst: PInstruction);
begin
  // ROR  Ev,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $56;
  PInst^.InstID := INST_ID_ROR;
  Decode_Ev_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCL_Ev_1(PInst: PInstruction);
begin
  // RCL  Ev,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $4F;
  PInst^.InstID := INST_ID_RCL;
  Decode_Ev_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCR_Ev_1(PInst: PInstruction);
begin
  // RCR  Ev,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $4F;
  PInst^.InstID := INST_ID_RCR;
  Decode_Ev_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHL_Ev_1(PInst: PInstruction);
begin
  // SHL  Ev,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5A;
  PInst^.InstID := INST_ID_SHL;
  Decode_Ev_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHR_Ev_1(PInst: PInstruction);
begin
  // SHR  Ev,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5A;
  PInst^.InstID := INST_ID_SHR;
  Decode_Ev_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAL_Ev_1(PInst: PInstruction);
begin
  // SAL  Ev,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5A;
  PInst^.InstID := INST_ID_SAL;
  Decode_Ev_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAR_Ev_1(PInst: PInstruction);
begin
  // SAR  Ev,1
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5A;
  PInst^.InstID := INST_ID_SAR;
  Decode_Ev_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROL_Eb_CL(PInst: PInstruction);
begin
  // ROL  Eb,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $57;
  PInst^.InstID := INST_ID_ROL;
  Decode_Eb_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROR_Eb_CL(PInst: PInstruction);
begin
  // ROR  Eb,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $57;
  PInst^.InstID := INST_ID_ROR;
  Decode_Eb_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCL_Eb_CL(PInst: PInstruction);
begin
  // RCL  Eb,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $50;
  PInst^.InstID := INST_ID_RCL;
  Decode_Eb_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCR_Eb_CL(PInst: PInstruction);
begin
  // RCR  Eb,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $50;
  PInst^.InstID := INST_ID_RCR;
  Decode_Eb_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHL_Eb_CL(PInst: PInstruction);
begin
  // SHL  Eb,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SHL;
  Decode_Eb_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHR_Eb_CL(PInst: PInstruction);
begin
  // SHR  Eb,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SHR;
  Decode_Eb_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAL_Eb_CL(PInst: PInstruction);
begin
  // SAL  Eb,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SAL;
  Decode_Eb_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAR_Eb_CL(PInst: PInstruction);
begin
  // SAR  Eb,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SAR;
  Decode_Eb_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROL_Ev_CL(PInst: PInstruction);
begin
  // ROL  Ev,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $57;
  PInst^.InstID := INST_ID_ROL;
  Decode_Ev_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_ROR_Ev_CL(PInst: PInstruction);
begin
  // ROR  Ev,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $57;
  PInst^.InstID := INST_ID_ROR;
  Decode_Ev_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ROR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCL_Ev_CL(PInst: PInstruction);
begin
  // RCL  Ev,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $50;
  PInst^.InstID := INST_ID_RCL;
  Decode_Ev_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RCR_Ev_CL(PInst: PInstruction);
begin
  // RCR  Ev,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_ROTATE;
  PInst^.FlagsIndex := $50;
  PInst^.InstID := INST_ID_RCR;
  Decode_Ev_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RCR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHL_Ev_CL(PInst: PInstruction);
begin
  // SHL  Ev,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SHL;
  Decode_Ev_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SHR_Ev_CL(PInst: PInstruction);
begin
  // SHR  Ev,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SHR;
  Decode_Ev_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SHR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAL_Ev_CL(PInst: PInstruction);
begin
  // SAL  Ev,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SAL;
  Decode_Ev_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SAR_Ev_CL(PInst: PInstruction);
begin
  // SAR  Ev,CL
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_SHIFT;
  PInst^.FlagsIndex := $5B;
  PInst^.InstID := INST_ID_SAR;
  Decode_Ev_CL(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SAR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_TEST_Eb_Ib(PInst: PInstruction);
begin
  // TEST  Eb,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $67;
  PInst^.InstID := INST_ID_TEST;
  Decode_Eb_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_TEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_NOT_Eb(PInst: PInstruction);
begin
  // NOT  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $48;
  PInst^.InstID := INST_ID_NOT;
  Decode_Eb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_NOT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_NEG_Eb(PInst: PInstruction);
begin
  // NEG  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $46;
  PInst^.InstID := INST_ID_NEG;
  Decode_Eb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_NEG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MUL_Eb(PInst: PInstruction);
begin
  // MUL  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $45;
  PInst^.InstID := INST_ID_MUL;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IMUL_Eb(PInst: PInstruction);
begin
  // IMUL  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $27;
  PInst^.InstID := INST_ID_IMUL;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DIV_Eb(PInst: PInstruction);
begin
  // DIV  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $20;
  PInst^.InstID := INST_ID_DIV;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IDIV_Eb(PInst: PInstruction);
begin
  // IDIV  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $26;
  PInst^.InstID := INST_ID_IDIV;
  Decode_Eb(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_TEST_Ev_Iz(PInst: PInstruction);
begin
  // TEST  Ev,Iz
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_BIT_BYTE or INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $67;
  PInst^.InstID := INST_ID_TEST;
  Decode_Ev_Iz(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_TEST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_NOT_Ev(PInst: PInstruction);
begin
  // NOT  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_LOGICAL;
  PInst^.FlagsIndex := $48;
  PInst^.InstID := INST_ID_NOT;
  Decode_Ev(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_NOT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_NEG_Ev(PInst: PInstruction);
begin
  // NEG  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $46;
  PInst^.InstID := INST_ID_NEG;
  Decode_Ev(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_NEG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_MUL_Ev(PInst: PInstruction);
begin
  // MUL  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $45;
  PInst^.InstID := INST_ID_MUL;
  Decode_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_MUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IMUL_Ev(PInst: PInstruction);
begin
  // IMUL  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $27;
  PInst^.InstID := INST_ID_IMUL;
  Decode_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DIV_Ev(PInst: PInstruction);
begin
  // DIV  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_UNSIGNED);
  PInst^.FlagsIndex := $20;
  PInst^.InstID := INST_ID_DIV;
  Decode_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_IDIV_Ev(PInst: PInstruction);
begin
  // IDIV  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY or
    INST_CATEGORY_SIGNED);
  PInst^.FlagsIndex := $26;
  PInst^.InstID := INST_ID_IDIV;
  Decode_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_IDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_Eb(PInst: PInstruction);
begin
  // INC  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_Eb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_Eb(PInst: PInstruction);
begin
  // DEC  Eb
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_Eb(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_INC_Ev(PInst: PInstruction);
begin
  // INC  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $29;
  PInst^.InstID := INST_ID_INC;
  Decode_Ev(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_DEC_Ev(PInst: PInstruction);
begin
  // DEC  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_BINARY);
  PInst^.FlagsIndex := $1F;
  PInst^.InstID := INST_ID_DEC;
  Decode_Ev(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_DEC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CALL_Ev_Df64(PInst: PInstruction);
begin
  // CALL  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $0D;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_CALL;
  DecodeBranch_Ev(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CALL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CALL_Mp_w_z(PInst: PInstruction);
begin
  // CALL  Mp(w:z)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_CONTROL_TRANSFER;
  PInst^.FlagsIndex := $0D;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_CALL;
  DecodeBranch_Mp_w_z(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CALL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JMP_Ev_Df64(PInst: PInstruction);
begin
  // JMP  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $34;
  PInst^.SetSizeToDf64();
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JMP;
  DecodeBranch_Ev(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_JMP_Mp_w_z(PInst: PInstruction);
begin
  // JMP  Mp(w:z)
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_CONTROL_TRANSFER);
  PInst^.FlagsIndex := $34;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JMP;
  DecodeBranch_Mp_w_z(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_PUSH_Ev_D64(PInst: PInstruction);
begin
  // PUSH  Ev
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_MISCELLANEOUS;
  PInst^.FlagsIndex := $4E;
  PInst^.SetSizeToD64();
  PInst^.InstID := INST_ID_PUSH;
  Decode_Ev(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_PUSH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_SWAPGS_void(PInst: PInstruction);
begin
  // SWAPGS  void
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_64_BIT;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SWAPGS;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SWAPGS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BT_Ev_Ib(PInst: PInstruction);
begin
  // BT  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0C;
  PInst^.InstID := INST_ID_BT;
  Decode_Ev_Ib(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BTS_Ev_Ib(PInst: PInstruction);
begin
  // BTS  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0C;
  PInst^.InstID := INST_ID_BTS;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BTS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BTR_Ev_Ib(PInst: PInstruction);
begin
  // BTR  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0C;
  PInst^.InstID := INST_ID_BTR;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BTR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_BTC_Ev_Ib(PInst: PInstruction);
begin
  // BTC  Ev,Ib
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_BIT_BYTE;
  PInst^.FlagsIndex := $0C;
  PInst^.InstID := INST_ID_BTC;
  Decode_Ev_Ib(PInst);
  ValidateLock(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_BTC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMPXCHG8B_Mq(PInst: PInstruction);
begin
  // CMPXCHG8B  Mq
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_DATA_TRANSFER;
  PInst^.FlagsIndex := $18;
  PInst^.InstID := INST_ID_CMPXCHG8B;
  Decode_Mq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMPXCHG8B);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_CMPXCHG16B_Mo(PInst: PInstruction);
begin
  // CMPXCHG16B  Mo
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := (INST_CATEGORY_64_BIT or INST_CATEGORY_COMPARE);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CMPXCHG16B;
  Decode_Mo(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CMPXCHG16B);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RDRAND_Rv(PInst: PInstruction);
begin
  // RDRAND  Rv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_RANDOM_NUMBER_GENERATOR;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_RDRAND;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RDRAND);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_GP_RDSEED_Rv(PInst: PInstruction);
begin
  // RDSEED  Rv
  PInst^.InstGroups := INST_GRP_GP;
  PInst^.InstCategory := INST_CATEGORY_RANDOM_NUMBER_GENERATOR;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_RDSEED;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RDSEED);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
