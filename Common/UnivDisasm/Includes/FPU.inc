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
// The Original Code is FPU.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_FPU_FILD_word_integer(PInst: PInstruction);
begin
  // FILD  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_INTEGER or INST_CATEGORY_LOAD or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FILD;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FILD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISTTP_word_integer(PInst: PInstruction);
begin
  // FISTTP  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISTTP;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISTTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIST_word_integer(PInst: PInstruction);
begin
  // FIST  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_INTEGER or INST_CATEGORY_STORE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIST;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISTP_word_integer(PInst: PInstruction);
begin
  // FISTP  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_INTEGER or INST_CATEGORY_STORE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISTP;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FBLD_packed_BCD(PInst: PInstruction);
begin
  // FBLD  packed-BCD
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FBLD;
  Decode_packed_BCD(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FBLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FILD_qword_integer(PInst: PInstruction);
begin
  // FILD  qword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_INTEGER or INST_CATEGORY_LOAD or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FILD;
  Decode_qword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FILD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FBSTP_packed_BCD(PInst: PInstruction);
begin
  // FBSTP  packed-BCD
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FBSTP;
  Decode_packed_BCD(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FBSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISTP_qword_integer(PInst: PInstruction);
begin
  // FISTP  qword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_INTEGER or INST_CATEGORY_STORE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISTP;
  Decode_qword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTSW_AX(PInst: PInstruction);
begin
  // FSTSW  AX
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTSW;
  Decode_AX(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMIP_ST_0_ST_0(PInst: PInstruction);
begin
  // FUCOMIP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMIP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMIP_ST_0_ST_1(PInst: PInstruction);
begin
  // FUCOMIP  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMIP;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMIP_ST_0_ST_2(PInst: PInstruction);
begin
  // FUCOMIP  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMIP;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMIP_ST_0_ST_3(PInst: PInstruction);
begin
  // FUCOMIP  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMIP;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMIP_ST_0_ST_4(PInst: PInstruction);
begin
  // FUCOMIP  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMIP;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMIP_ST_0_ST_5(PInst: PInstruction);
begin
  // FUCOMIP  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMIP;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMIP_ST_0_ST_6(PInst: PInstruction);
begin
  // FUCOMIP  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMIP;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMIP_ST_0_ST_7(PInst: PInstruction);
begin
  // FUCOMIP  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMIP;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMIP_ST_0_ST_0(PInst: PInstruction);
begin
  // FCOMIP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMIP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMIP_ST_0_ST_1(PInst: PInstruction);
begin
  // FCOMIP  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMIP;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMIP_ST_0_ST_2(PInst: PInstruction);
begin
  // FCOMIP  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMIP;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMIP_ST_0_ST_3(PInst: PInstruction);
begin
  // FCOMIP  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMIP;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMIP_ST_0_ST_4(PInst: PInstruction);
begin
  // FCOMIP  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMIP;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMIP_ST_0_ST_5(PInst: PInstruction);
begin
  // FCOMIP  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMIP;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMIP_ST_0_ST_6(PInst: PInstruction);
begin
  // FCOMIP  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMIP;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMIP_ST_0_ST_7(PInst: PInstruction);
begin
  // FCOMIP  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMIP;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMIP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FILD_dword_integer(PInst: PInstruction);
begin
  // FILD  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_INTEGER or INST_CATEGORY_LOAD or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FILD;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FILD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISTTP_dword_integer(PInst: PInstruction);
begin
  // FISTTP  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISTTP;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISTTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIST_dword_integer(PInst: PInstruction);
begin
  // FIST  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_INTEGER or INST_CATEGORY_STORE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIST;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISTP_dword_integer(PInst: PInstruction);
begin
  // FISTP  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_INTEGER or INST_CATEGORY_STORE or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISTP;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_extended_real(PInst: PInstruction);
begin
  // FLD  extended-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_extended_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_extended_real(PInst: PInstruction);
begin
  // FSTP  extended-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_extended_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNB_ST_0_ST_0(PInst: PInstruction);
begin
  // FCMOVNB  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNB;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNB_ST_0_ST_1(PInst: PInstruction);
begin
  // FCMOVNB  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNB;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNB_ST_0_ST_2(PInst: PInstruction);
begin
  // FCMOVNB  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNB;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNB_ST_0_ST_3(PInst: PInstruction);
begin
  // FCMOVNB  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNB;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNB_ST_0_ST_4(PInst: PInstruction);
begin
  // FCMOVNB  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNB;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNB_ST_0_ST_5(PInst: PInstruction);
begin
  // FCMOVNB  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNB;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNB_ST_0_ST_6(PInst: PInstruction);
begin
  // FCMOVNB  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNB;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNB_ST_0_ST_7(PInst: PInstruction);
begin
  // FCMOVNB  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNB;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNE_ST_0_ST_0(PInst: PInstruction);
begin
  // FCMOVNE  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNE;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNE_ST_0_ST_1(PInst: PInstruction);
begin
  // FCMOVNE  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNE;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNE_ST_0_ST_2(PInst: PInstruction);
begin
  // FCMOVNE  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNE;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNE_ST_0_ST_3(PInst: PInstruction);
begin
  // FCMOVNE  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNE;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNE_ST_0_ST_4(PInst: PInstruction);
begin
  // FCMOVNE  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNE;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNE_ST_0_ST_5(PInst: PInstruction);
begin
  // FCMOVNE  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNE;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNE_ST_0_ST_6(PInst: PInstruction);
begin
  // FCMOVNE  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNE;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNE_ST_0_ST_7(PInst: PInstruction);
begin
  // FCMOVNE  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNE;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNBE_ST_0_ST_0(PInst: PInstruction);
begin
  // FCMOVNBE  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNBE;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNBE_ST_0_ST_1(PInst: PInstruction);
begin
  // FCMOVNBE  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNBE;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNBE_ST_0_ST_2(PInst: PInstruction);
begin
  // FCMOVNBE  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNBE;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNBE_ST_0_ST_3(PInst: PInstruction);
begin
  // FCMOVNBE  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNBE;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNBE_ST_0_ST_4(PInst: PInstruction);
begin
  // FCMOVNBE  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNBE;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNBE_ST_0_ST_5(PInst: PInstruction);
begin
  // FCMOVNBE  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNBE;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNBE_ST_0_ST_6(PInst: PInstruction);
begin
  // FCMOVNBE  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNBE;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNBE_ST_0_ST_7(PInst: PInstruction);
begin
  // FCMOVNBE  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNBE;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNU_ST_0_ST_0(PInst: PInstruction);
begin
  // FCMOVNU  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNU;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNU_ST_0_ST_1(PInst: PInstruction);
begin
  // FCMOVNU  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNU;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNU_ST_0_ST_2(PInst: PInstruction);
begin
  // FCMOVNU  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNU;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNU_ST_0_ST_3(PInst: PInstruction);
begin
  // FCMOVNU  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNU;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNU_ST_0_ST_4(PInst: PInstruction);
begin
  // FCMOVNU  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNU;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNU_ST_0_ST_5(PInst: PInstruction);
begin
  // FCMOVNU  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNU;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNU_ST_0_ST_6(PInst: PInstruction);
begin
  // FCMOVNU  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNU;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVNU_ST_0_ST_7(PInst: PInstruction);
begin
  // FCMOVNU  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVNU;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVNU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCLEX_void(PInst: PInstruction);
begin
  // FCLEX  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCLEX;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCLEX);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FINIT_void(PInst: PInstruction);
begin
  // FINIT  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FINIT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FINIT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMI_ST_0_ST_0(PInst: PInstruction);
begin
  // FUCOMI  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMI;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMI_ST_0_ST_1(PInst: PInstruction);
begin
  // FUCOMI  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMI;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMI_ST_0_ST_2(PInst: PInstruction);
begin
  // FUCOMI  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMI;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMI_ST_0_ST_3(PInst: PInstruction);
begin
  // FUCOMI  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMI;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMI_ST_0_ST_4(PInst: PInstruction);
begin
  // FUCOMI  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMI;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMI_ST_0_ST_5(PInst: PInstruction);
begin
  // FUCOMI  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMI;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMI_ST_0_ST_6(PInst: PInstruction);
begin
  // FUCOMI  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMI;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMI_ST_0_ST_7(PInst: PInstruction);
begin
  // FUCOMI  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FUCOMI;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMI_ST_0_ST_0(PInst: PInstruction);
begin
  // FCOMI  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMI;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMI_ST_0_ST_1(PInst: PInstruction);
begin
  // FCOMI  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMI;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMI_ST_0_ST_2(PInst: PInstruction);
begin
  // FCOMI  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMI;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMI_ST_0_ST_3(PInst: PInstruction);
begin
  // FCOMI  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMI;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMI_ST_0_ST_4(PInst: PInstruction);
begin
  // FCOMI  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMI;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMI_ST_0_ST_5(PInst: PInstruction);
begin
  // FCOMI  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMI;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMI_ST_0_ST_6(PInst: PInstruction);
begin
  // FCOMI  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMI;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMI_ST_0_ST_7(PInst: PInstruction);
begin
  // FCOMI  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $24;
  PInst^.InstID := INST_ID_FCOMI;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIADD_dword_integer(PInst: PInstruction);
begin
  // FIADD  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIADD;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIMUL_dword_integer(PInst: PInstruction);
begin
  // FIMUL  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIMUL;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FICOM_dword_integer(PInst: PInstruction);
begin
  // FICOM  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FICOM;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FICOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FICOMP_dword_integer(PInst: PInstruction);
begin
  // FICOMP  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FICOMP;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FICOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISUB_dword_integer(PInst: PInstruction);
begin
  // FISUB  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISUB;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISUBR_dword_integer(PInst: PInstruction);
begin
  // FISUBR  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISUBR;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIDIV_dword_integer(PInst: PInstruction);
begin
  // FIDIV  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIDIV;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIDIVR_dword_integer(PInst: PInstruction);
begin
  // FIDIVR  dword-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIDIVR;
  Decode_dword_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVB_ST_0_ST_0(PInst: PInstruction);
begin
  // FCMOVB  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVB;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVB_ST_0_ST_1(PInst: PInstruction);
begin
  // FCMOVB  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVB;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVB_ST_0_ST_2(PInst: PInstruction);
begin
  // FCMOVB  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVB;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVB_ST_0_ST_3(PInst: PInstruction);
begin
  // FCMOVB  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVB;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVB_ST_0_ST_4(PInst: PInstruction);
begin
  // FCMOVB  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVB;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVB_ST_0_ST_5(PInst: PInstruction);
begin
  // FCMOVB  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVB;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVB_ST_0_ST_6(PInst: PInstruction);
begin
  // FCMOVB  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVB;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVB_ST_0_ST_7(PInst: PInstruction);
begin
  // FCMOVB  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVB;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVE_ST_0_ST_0(PInst: PInstruction);
begin
  // FCMOVE  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVE;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVE_ST_0_ST_1(PInst: PInstruction);
begin
  // FCMOVE  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVE;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVE_ST_0_ST_2(PInst: PInstruction);
begin
  // FCMOVE  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVE;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVE_ST_0_ST_3(PInst: PInstruction);
begin
  // FCMOVE  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVE;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVE_ST_0_ST_4(PInst: PInstruction);
begin
  // FCMOVE  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVE;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVE_ST_0_ST_5(PInst: PInstruction);
begin
  // FCMOVE  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVE;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVE_ST_0_ST_6(PInst: PInstruction);
begin
  // FCMOVE  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVE;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVE_ST_0_ST_7(PInst: PInstruction);
begin
  // FCMOVE  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVE;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVBE_ST_0_ST_0(PInst: PInstruction);
begin
  // FCMOVBE  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVBE;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVBE_ST_0_ST_1(PInst: PInstruction);
begin
  // FCMOVBE  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVBE;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVBE_ST_0_ST_2(PInst: PInstruction);
begin
  // FCMOVBE  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVBE;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVBE_ST_0_ST_3(PInst: PInstruction);
begin
  // FCMOVBE  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVBE;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVBE_ST_0_ST_4(PInst: PInstruction);
begin
  // FCMOVBE  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVBE;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVBE_ST_0_ST_5(PInst: PInstruction);
begin
  // FCMOVBE  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVBE;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVBE_ST_0_ST_6(PInst: PInstruction);
begin
  // FCMOVBE  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVBE;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVBE_ST_0_ST_7(PInst: PInstruction);
begin
  // FCMOVBE  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVBE;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVBE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVU_ST_0_ST_0(PInst: PInstruction);
begin
  // FCMOVU  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVU;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVU_ST_0_ST_1(PInst: PInstruction);
begin
  // FCMOVU  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVU;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVU_ST_0_ST_2(PInst: PInstruction);
begin
  // FCMOVU  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVU;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVU_ST_0_ST_3(PInst: PInstruction);
begin
  // FCMOVU  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVU;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVU_ST_0_ST_4(PInst: PInstruction);
begin
  // FCMOVU  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVU;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVU_ST_0_ST_5(PInst: PInstruction);
begin
  // FCMOVU  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVU;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVU_ST_0_ST_6(PInst: PInstruction);
begin
  // FCMOVU  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVU;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCMOVU_ST_0_ST_7(PInst: PInstruction);
begin
  // FCMOVU  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONDITIONAL or
    INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_FLOATING_POINT or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $23;
  PInst^.InstID := INST_ID_FCMOVU;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCMOVU);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMPP_void(PInst: PInstruction);
begin
  // FUCOMPP  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMPP;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMPP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_WAIT_void(PInst: PInstruction);
begin
  // WAIT  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $6A;
  PInst^.InstID := INST_ID_WAIT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_WAIT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_single_real(PInst: PInstruction);
begin
  // FLD  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_single_real(PInst: PInstruction);
begin
  // FST  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_single_real(PInst: PInstruction);
begin
  // FSTP  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLDENV_M14_M28(PInst: PInstruction);
begin
  // FLDENV  M14/M28
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLDENV;
  Decode_M14_M28(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLDENV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLDCW_Mw(PInst: PInstruction);
begin
  // FLDCW  Mw
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLDCW;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLDCW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTENV_M14_M28(PInst: PInstruction);
begin
  // FSTENV  M14/M28
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTENV;
  Decode_M14_M28(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTENV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTCW_Mw(PInst: PInstruction);
begin
  // FSTCW  Mw
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTCW;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTCW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_ST_0_ST_0(PInst: PInstruction);
begin
  // FLD  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_ST_0_ST_1(PInst: PInstruction);
begin
  // FLD  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_ST_0_ST_2(PInst: PInstruction);
begin
  // FLD  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_ST_0_ST_3(PInst: PInstruction);
begin
  // FLD  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_ST_0_ST_4(PInst: PInstruction);
begin
  // FLD  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_ST_0_ST_5(PInst: PInstruction);
begin
  // FLD  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_ST_0_ST_6(PInst: PInstruction);
begin
  // FLD  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_ST_0_ST_7(PInst: PInstruction);
begin
  // FLD  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXCH_ST_0_ST_0(PInst: PInstruction);
begin
  // FXCH  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXCH;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXCH_ST_0_ST_1(PInst: PInstruction);
begin
  // FXCH  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXCH;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXCH_ST_0_ST_2(PInst: PInstruction);
begin
  // FXCH  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXCH;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXCH_ST_0_ST_3(PInst: PInstruction);
begin
  // FXCH  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXCH;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXCH_ST_0_ST_4(PInst: PInstruction);
begin
  // FXCH  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXCH;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXCH_ST_0_ST_5(PInst: PInstruction);
begin
  // FXCH  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXCH;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXCH_ST_0_ST_6(PInst: PInstruction);
begin
  // FXCH  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXCH;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXCH_ST_0_ST_7(PInst: PInstruction);
begin
  // FXCH  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXCH;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FNOP_void(PInst: PInstruction);
begin
  // FNOP  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FNOP;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FNOP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCHS_void(PInst: PInstruction);
begin
  // FCHS  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCHS;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCHS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FABS_void(PInst: PInstruction);
begin
  // FABS  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FABS;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FABS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FTST_void(PInst: PInstruction);
begin
  // FTST  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FTST;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FTST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXAM_void(PInst: PInstruction);
begin
  // FXAM  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXAM;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXAM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD1_void(PInst: PInstruction);
begin
  // FLD1  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONSTANTS or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD1;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLDL2T_void(PInst: PInstruction);
begin
  // FLDL2T  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONSTANTS or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLDL2T;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLDL2T);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLDL2E_void(PInst: PInstruction);
begin
  // FLDL2E  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONSTANTS or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLDL2E;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLDL2E);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLDPI_void(PInst: PInstruction);
begin
  // FLDPI  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONSTANTS or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLDPI;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLDPI);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLDLG2_void(PInst: PInstruction);
begin
  // FLDLG2  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONSTANTS or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLDLG2;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLDLG2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLDLN2_void(PInst: PInstruction);
begin
  // FLDLN2  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONSTANTS or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLDLN2;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLDLN2);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLDZ_void(PInst: PInstruction);
begin
  // FLDZ  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONSTANTS or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLDZ;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLDZ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_F2XM1_void(PInst: PInstruction);
begin
  // F2XM1  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_TRANSCENDENTAL or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_F2XM1;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_F2XM1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FYL2X_void(PInst: PInstruction);
begin
  // FYL2X  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_TRANSCENDENTAL or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FYL2X;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FYL2X);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FPTAN_void(PInst: PInstruction);
begin
  // FPTAN  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_TRANSCENDENTAL or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FPTAN;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FPTAN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FPATAN_void(PInst: PInstruction);
begin
  // FPATAN  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_TRANSCENDENTAL or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FPATAN;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FPATAN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXTRACT_void(PInst: PInstruction);
begin
  // FXTRACT  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXTRACT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXTRACT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FPREM1_void(PInst: PInstruction);
begin
  // FPREM1  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FPREM1;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FPREM1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDECSTP_void(PInst: PInstruction);
begin
  // FDECSTP  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDECSTP;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDECSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FINCSTP_void(PInst: PInstruction);
begin
  // FINCSTP  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FINCSTP;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FINCSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FPREM_void(PInst: PInstruction);
begin
  // FPREM  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FPREM;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FPREM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FYL2XP1_void(PInst: PInstruction);
begin
  // FYL2XP1  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_TRANSCENDENTAL or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FYL2XP1;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FYL2XP1);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSQRT_void(PInst: PInstruction);
begin
  // FSQRT  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSQRT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSQRT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSINCOS_void(PInst: PInstruction);
begin
  // FSINCOS  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_TRANSCENDENTAL or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSINCOS;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSINCOS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FRNDINT_void(PInst: PInstruction);
begin
  // FRNDINT  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FRNDINT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FRNDINT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSCALE_void(PInst: PInstruction);
begin
  // FSCALE  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSCALE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSCALE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSIN_void(PInst: PInstruction);
begin
  // FSIN  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_TRANSCENDENTAL or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSIN;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSIN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOS_void(PInst: PInstruction);
begin
  // FCOS  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_TRANSCENDENTAL or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOS;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FLD_double_real(PInst: PInstruction);
begin
  // FLD  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_LOAD or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FLD;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISTTP_integer64(PInst: PInstruction);
begin
  // FISTTP  integer64
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONVERT or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISTTP;
  Decode_integer64(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISTTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_double_real(PInst: PInstruction);
begin
  // FST  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_double_real(PInst: PInstruction);
begin
  // FSTP  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FRSTOR_M98_M108(PInst: PInstruction);
begin
  // FRSTOR  M98/M108
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FRSTOR;
  Decode_M98_M108(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FRSTOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSAVE_M98_M108(PInst: PInstruction);
begin
  // FSAVE  M98/M108
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSAVE;
  Decode_M98_M108(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSAVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTSW_Mw(PInst: PInstruction);
begin
  // FSTSW  Mw
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTSW;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FFREE_ST_0(PInst: PInstruction);
begin
  // FFREE  ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FFREE;
  Decode_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FFREE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FFREE_ST_1(PInst: PInstruction);
begin
  // FFREE  ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FFREE;
  Decode_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FFREE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FFREE_ST_2(PInst: PInstruction);
begin
  // FFREE  ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FFREE;
  Decode_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FFREE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FFREE_ST_3(PInst: PInstruction);
begin
  // FFREE  ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FFREE;
  Decode_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FFREE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FFREE_ST_4(PInst: PInstruction);
begin
  // FFREE  ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FFREE;
  Decode_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FFREE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FFREE_ST_5(PInst: PInstruction);
begin
  // FFREE  ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FFREE;
  Decode_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FFREE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FFREE_ST_6(PInst: PInstruction);
begin
  // FFREE  ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FFREE;
  Decode_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FFREE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FFREE_ST_7(PInst: PInstruction);
begin
  // FFREE  ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_CONTROL or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FFREE;
  Decode_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FFREE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_ST_0(PInst: PInstruction);
begin
  // FST  ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_ST_1(PInst: PInstruction);
begin
  // FST  ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_ST_2(PInst: PInstruction);
begin
  // FST  ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_ST_3(PInst: PInstruction);
begin
  // FST  ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_ST_4(PInst: PInstruction);
begin
  // FST  ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_ST_5(PInst: PInstruction);
begin
  // FST  ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_ST_6(PInst: PInstruction);
begin
  // FST  ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FST_ST_7(PInst: PInstruction);
begin
  // FST  ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FST;
  Decode_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_ST_0(PInst: PInstruction);
begin
  // FSTP  ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_ST_1(PInst: PInstruction);
begin
  // FSTP  ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_ST_2(PInst: PInstruction);
begin
  // FSTP  ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_ST_3(PInst: PInstruction);
begin
  // FSTP  ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_ST_4(PInst: PInstruction);
begin
  // FSTP  ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_ST_5(PInst: PInstruction);
begin
  // FSTP  ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_ST_6(PInst: PInstruction);
begin
  // FSTP  ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSTP_ST_7(PInst: PInstruction);
begin
  // FSTP  ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_DATA_TRANSFER or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_STORE or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSTP;
  Decode_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSTP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOM_ST_0_ST_0(PInst: PInstruction);
begin
  // FUCOM  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOM;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOM_ST_1_ST_0(PInst: PInstruction);
begin
  // FUCOM  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOM;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOM_ST_2_ST_0(PInst: PInstruction);
begin
  // FUCOM  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOM;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOM_ST_3_ST_0(PInst: PInstruction);
begin
  // FUCOM  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOM;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOM_ST_4_ST_0(PInst: PInstruction);
begin
  // FUCOM  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOM;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOM_ST_5_ST_0(PInst: PInstruction);
begin
  // FUCOM  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOM;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOM_ST_6_ST_0(PInst: PInstruction);
begin
  // FUCOM  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOM;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOM_ST_7_ST_0(PInst: PInstruction);
begin
  // FUCOM  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOM;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMP_ST_0(PInst: PInstruction);
begin
  // FUCOMP  ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMP;
  Decode_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMP_ST_1(PInst: PInstruction);
begin
  // FUCOMP  ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMP;
  Decode_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMP_ST_2(PInst: PInstruction);
begin
  // FUCOMP  ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMP;
  Decode_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMP_ST_3(PInst: PInstruction);
begin
  // FUCOMP  ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMP;
  Decode_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMP_ST_4(PInst: PInstruction);
begin
  // FUCOMP  ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMP;
  Decode_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMP_ST_5(PInst: PInstruction);
begin
  // FUCOMP  ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMP;
  Decode_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMP_ST_6(PInst: PInstruction);
begin
  // FUCOMP  ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMP;
  Decode_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FUCOMP_ST_7(PInst: PInstruction);
begin
  // FUCOMP  ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FUCOMP;
  Decode_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FUCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIADD_word_integer(PInst: PInstruction);
begin
  // FIADD  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIADD;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIMUL_word_integer(PInst: PInstruction);
begin
  // FIMUL  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIMUL;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FICOM_word_integer(PInst: PInstruction);
begin
  // FICOM  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FICOM;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FICOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FICOMP_word_integer(PInst: PInstruction);
begin
  // FICOMP  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FICOMP;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FICOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISUB_word_integer(PInst: PInstruction);
begin
  // FISUB  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISUB;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FISUBR_word_integer(PInst: PInstruction);
begin
  // FISUBR  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FISUBR;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FISUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIDIV_word_integer(PInst: PInstruction);
begin
  // FIDIV  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIDIV;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FIDIVR_word_integer(PInst: PInstruction);
begin
  // FIDIVR  word-integer
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or INST_CATEGORY_INTEGER or
    INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FIDIVR;
  Decode_word_integer(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FIDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADDP_ST_0_ST_0(PInst: PInstruction);
begin
  // FADDP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADDP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADDP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADDP_ST_1_ST_0(PInst: PInstruction);
begin
  // FADDP  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADDP;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADDP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADDP_ST_2_ST_0(PInst: PInstruction);
begin
  // FADDP  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADDP;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADDP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADDP_ST_3_ST_0(PInst: PInstruction);
begin
  // FADDP  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADDP;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADDP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADDP_ST_4_ST_0(PInst: PInstruction);
begin
  // FADDP  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADDP;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADDP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADDP_ST_5_ST_0(PInst: PInstruction);
begin
  // FADDP  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADDP;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADDP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADDP_ST_6_ST_0(PInst: PInstruction);
begin
  // FADDP  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADDP;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADDP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADDP_ST_7_ST_0(PInst: PInstruction);
begin
  // FADDP  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADDP;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADDP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMULP_ST_0_ST_0(PInst: PInstruction);
begin
  // FMULP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMULP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMULP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMULP_ST_1_ST_0(PInst: PInstruction);
begin
  // FMULP  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMULP;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMULP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMULP_ST_2_ST_0(PInst: PInstruction);
begin
  // FMULP  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMULP;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMULP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMULP_ST_3_ST_0(PInst: PInstruction);
begin
  // FMULP  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMULP;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMULP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMULP_ST_4_ST_0(PInst: PInstruction);
begin
  // FMULP  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMULP;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMULP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMULP_ST_5_ST_0(PInst: PInstruction);
begin
  // FMULP  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMULP;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMULP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMULP_ST_6_ST_0(PInst: PInstruction);
begin
  // FMULP  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMULP;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMULP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMULP_ST_7_ST_0(PInst: PInstruction);
begin
  // FMULP  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMULP;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMULP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMPP_void(PInst: PInstruction);
begin
  // FCOMPP  void
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMPP;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMPP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBRP_ST_0_ST_0(PInst: PInstruction);
begin
  // FSUBRP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBRP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBRP_ST_1_ST_0(PInst: PInstruction);
begin
  // FSUBRP  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBRP;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBRP_ST_2_ST_0(PInst: PInstruction);
begin
  // FSUBRP  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBRP;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBRP_ST_3_ST_0(PInst: PInstruction);
begin
  // FSUBRP  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBRP;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBRP_ST_4_ST_0(PInst: PInstruction);
begin
  // FSUBRP  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBRP;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBRP_ST_5_ST_0(PInst: PInstruction);
begin
  // FSUBRP  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBRP;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBRP_ST_6_ST_0(PInst: PInstruction);
begin
  // FSUBRP  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBRP;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBRP_ST_7_ST_0(PInst: PInstruction);
begin
  // FSUBRP  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBRP;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBP_ST_0_ST_0(PInst: PInstruction);
begin
  // FSUBP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBP_ST_1_ST_0(PInst: PInstruction);
begin
  // FSUBP  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBP;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBP_ST_2_ST_0(PInst: PInstruction);
begin
  // FSUBP  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBP;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBP_ST_3_ST_0(PInst: PInstruction);
begin
  // FSUBP  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBP;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBP_ST_4_ST_0(PInst: PInstruction);
begin
  // FSUBP  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBP;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBP_ST_5_ST_0(PInst: PInstruction);
begin
  // FSUBP  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBP;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBP_ST_6_ST_0(PInst: PInstruction);
begin
  // FSUBP  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBP;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBP_ST_7_ST_0(PInst: PInstruction);
begin
  // FSUBP  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBP;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVRP_ST_0_ST_0(PInst: PInstruction);
begin
  // FDIVRP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVRP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVRP_ST_1_ST_0(PInst: PInstruction);
begin
  // FDIVRP  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVRP;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVRP_ST_2_ST_0(PInst: PInstruction);
begin
  // FDIVRP  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVRP;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVRP_ST_3_ST_0(PInst: PInstruction);
begin
  // FDIVRP  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVRP;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVRP_ST_4_ST_0(PInst: PInstruction);
begin
  // FDIVRP  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVRP;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVRP_ST_5_ST_0(PInst: PInstruction);
begin
  // FDIVRP  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVRP;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVRP_ST_6_ST_0(PInst: PInstruction);
begin
  // FDIVRP  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVRP;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVRP_ST_7_ST_0(PInst: PInstruction);
begin
  // FDIVRP  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVRP;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVRP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVP_ST_0_ST_0(PInst: PInstruction);
begin
  // FDIVP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVP_ST_1_ST_0(PInst: PInstruction);
begin
  // FDIVP  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVP;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVP_ST_2_ST_0(PInst: PInstruction);
begin
  // FDIVP  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVP;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVP_ST_3_ST_0(PInst: PInstruction);
begin
  // FDIVP  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVP;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVP_ST_4_ST_0(PInst: PInstruction);
begin
  // FDIVP  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVP;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVP_ST_5_ST_0(PInst: PInstruction);
begin
  // FDIVP  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVP;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVP_ST_6_ST_0(PInst: PInstruction);
begin
  // FDIVP  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVP;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVP_ST_7_ST_0(PInst: PInstruction);
begin
  // FDIVP  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVP;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_single_real(PInst: PInstruction);
begin
  // FADD  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_single_real(PInst: PInstruction);
begin
  // FMUL  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_single_real(PInst: PInstruction);
begin
  // FCOM  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_single_real(PInst: PInstruction);
begin
  // FCOMP  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_single_real(PInst: PInstruction);
begin
  // FSUB  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_single_real(PInst: PInstruction);
begin
  // FSUBR  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_single_real(PInst: PInstruction);
begin
  // FDIV  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_single_real(PInst: PInstruction);
begin
  // FDIVR  single-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_single_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_0_ST_0(PInst: PInstruction);
begin
  // FADD  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_0_ST_1(PInst: PInstruction);
begin
  // FADD  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_0_ST_2(PInst: PInstruction);
begin
  // FADD  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_0_ST_3(PInst: PInstruction);
begin
  // FADD  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_0_ST_4(PInst: PInstruction);
begin
  // FADD  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_0_ST_5(PInst: PInstruction);
begin
  // FADD  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_0_ST_6(PInst: PInstruction);
begin
  // FADD  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_0_ST_7(PInst: PInstruction);
begin
  // FADD  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_0_ST_0(PInst: PInstruction);
begin
  // FMUL  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_0_ST_1(PInst: PInstruction);
begin
  // FMUL  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_0_ST_2(PInst: PInstruction);
begin
  // FMUL  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_0_ST_3(PInst: PInstruction);
begin
  // FMUL  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_0_ST_4(PInst: PInstruction);
begin
  // FMUL  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_0_ST_5(PInst: PInstruction);
begin
  // FMUL  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_0_ST_6(PInst: PInstruction);
begin
  // FMUL  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_0_ST_7(PInst: PInstruction);
begin
  // FMUL  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_ST_0_ST_0(PInst: PInstruction);
begin
  // FCOM  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_ST_0_ST_1(PInst: PInstruction);
begin
  // FCOM  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_ST_0_ST_2(PInst: PInstruction);
begin
  // FCOM  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_ST_0_ST_3(PInst: PInstruction);
begin
  // FCOM  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_ST_0_ST_4(PInst: PInstruction);
begin
  // FCOM  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_ST_0_ST_5(PInst: PInstruction);
begin
  // FCOM  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_ST_0_ST_6(PInst: PInstruction);
begin
  // FCOM  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_ST_0_ST_7(PInst: PInstruction);
begin
  // FCOM  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_ST_0_ST_0(PInst: PInstruction);
begin
  // FCOMP  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_ST_0_ST_1(PInst: PInstruction);
begin
  // FCOMP  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_ST_0_ST_2(PInst: PInstruction);
begin
  // FCOMP  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_ST_0_ST_3(PInst: PInstruction);
begin
  // FCOMP  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_ST_0_ST_4(PInst: PInstruction);
begin
  // FCOMP  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_ST_0_ST_5(PInst: PInstruction);
begin
  // FCOMP  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_ST_0_ST_6(PInst: PInstruction);
begin
  // FCOMP  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_ST_0_ST_7(PInst: PInstruction);
begin
  // FCOMP  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_0_ST_0(PInst: PInstruction);
begin
  // FSUB  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_0_ST_1(PInst: PInstruction);
begin
  // FSUB  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_0_ST_2(PInst: PInstruction);
begin
  // FSUB  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_0_ST_3(PInst: PInstruction);
begin
  // FSUB  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_0_ST_4(PInst: PInstruction);
begin
  // FSUB  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_0_ST_5(PInst: PInstruction);
begin
  // FSUB  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_0_ST_6(PInst: PInstruction);
begin
  // FSUB  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_0_ST_7(PInst: PInstruction);
begin
  // FSUB  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_0_ST_0(PInst: PInstruction);
begin
  // FSUBR  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_0_ST_1(PInst: PInstruction);
begin
  // FSUBR  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_0_ST_2(PInst: PInstruction);
begin
  // FSUBR  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_0_ST_3(PInst: PInstruction);
begin
  // FSUBR  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_0_ST_4(PInst: PInstruction);
begin
  // FSUBR  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_0_ST_5(PInst: PInstruction);
begin
  // FSUBR  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_0_ST_6(PInst: PInstruction);
begin
  // FSUBR  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_0_ST_7(PInst: PInstruction);
begin
  // FSUBR  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_0_ST_0(PInst: PInstruction);
begin
  // FDIV  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_0_ST_1(PInst: PInstruction);
begin
  // FDIV  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_0_ST_2(PInst: PInstruction);
begin
  // FDIV  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_0_ST_3(PInst: PInstruction);
begin
  // FDIV  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_0_ST_4(PInst: PInstruction);
begin
  // FDIV  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_0_ST_5(PInst: PInstruction);
begin
  // FDIV  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_0_ST_6(PInst: PInstruction);
begin
  // FDIV  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_0_ST_7(PInst: PInstruction);
begin
  // FDIV  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_0_ST_0(PInst: PInstruction);
begin
  // FDIVR  ST(0),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_0_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_0_ST_1(PInst: PInstruction);
begin
  // FDIVR  ST(0),ST(1)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_0_ST_1(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_0_ST_2(PInst: PInstruction);
begin
  // FDIVR  ST(0),ST(2)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_0_ST_2(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_0_ST_3(PInst: PInstruction);
begin
  // FDIVR  ST(0),ST(3)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_0_ST_3(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_0_ST_4(PInst: PInstruction);
begin
  // FDIVR  ST(0),ST(4)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_0_ST_4(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_0_ST_5(PInst: PInstruction);
begin
  // FDIVR  ST(0),ST(5)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_0_ST_5(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_0_ST_6(PInst: PInstruction);
begin
  // FDIVR  ST(0),ST(6)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_0_ST_6(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_0_ST_7(PInst: PInstruction);
begin
  // FDIVR  ST(0),ST(7)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_0_ST_7(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_double_real(PInst: PInstruction);
begin
  // FADD  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_double_real(PInst: PInstruction);
begin
  // FMUL  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOM_double_real(PInst: PInstruction);
begin
  // FCOM  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOM;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOM);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FCOMP_double_real(PInst: PInstruction);
begin
  // FCOMP  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_COMPARE or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FCOMP;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FCOMP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_double_real(PInst: PInstruction);
begin
  // FSUB  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_double_real(PInst: PInstruction);
begin
  // FSUBR  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_double_real(PInst: PInstruction);
begin
  // FDIV  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_double_real(PInst: PInstruction);
begin
  // FDIVR  double-real
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_double_real(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_1_ST_0(PInst: PInstruction);
begin
  // FADD  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_2_ST_0(PInst: PInstruction);
begin
  // FADD  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_3_ST_0(PInst: PInstruction);
begin
  // FADD  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_4_ST_0(PInst: PInstruction);
begin
  // FADD  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_5_ST_0(PInst: PInstruction);
begin
  // FADD  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_6_ST_0(PInst: PInstruction);
begin
  // FADD  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FADD_ST_7_ST_0(PInst: PInstruction);
begin
  // FADD  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FADD;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FADD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_1_ST_0(PInst: PInstruction);
begin
  // FMUL  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_2_ST_0(PInst: PInstruction);
begin
  // FMUL  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_3_ST_0(PInst: PInstruction);
begin
  // FMUL  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_4_ST_0(PInst: PInstruction);
begin
  // FMUL  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_5_ST_0(PInst: PInstruction);
begin
  // FMUL  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_6_ST_0(PInst: PInstruction);
begin
  // FMUL  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FMUL_ST_7_ST_0(PInst: PInstruction);
begin
  // FMUL  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FMUL;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FMUL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_1_ST_0(PInst: PInstruction);
begin
  // FSUBR  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_2_ST_0(PInst: PInstruction);
begin
  // FSUBR  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_3_ST_0(PInst: PInstruction);
begin
  // FSUBR  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_4_ST_0(PInst: PInstruction);
begin
  // FSUBR  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_5_ST_0(PInst: PInstruction);
begin
  // FSUBR  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_6_ST_0(PInst: PInstruction);
begin
  // FSUBR  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUBR_ST_7_ST_0(PInst: PInstruction);
begin
  // FSUBR  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUBR;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUBR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_1_ST_0(PInst: PInstruction);
begin
  // FSUB  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_2_ST_0(PInst: PInstruction);
begin
  // FSUB  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_3_ST_0(PInst: PInstruction);
begin
  // FSUB  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_4_ST_0(PInst: PInstruction);
begin
  // FSUB  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_5_ST_0(PInst: PInstruction);
begin
  // FSUB  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_6_ST_0(PInst: PInstruction);
begin
  // FSUB  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FSUB_ST_7_ST_0(PInst: PInstruction);
begin
  // FSUB  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FSUB;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FSUB);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_1_ST_0(PInst: PInstruction);
begin
  // FDIVR  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_2_ST_0(PInst: PInstruction);
begin
  // FDIVR  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_3_ST_0(PInst: PInstruction);
begin
  // FDIVR  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_4_ST_0(PInst: PInstruction);
begin
  // FDIVR  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_5_ST_0(PInst: PInstruction);
begin
  // FDIVR  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_6_ST_0(PInst: PInstruction);
begin
  // FDIVR  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIVR_ST_7_ST_0(PInst: PInstruction);
begin
  // FDIVR  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIVR;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIVR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_1_ST_0(PInst: PInstruction);
begin
  // FDIV  ST(1),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_1_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_2_ST_0(PInst: PInstruction);
begin
  // FDIV  ST(2),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_2_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_3_ST_0(PInst: PInstruction);
begin
  // FDIV  ST(3),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_3_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_4_ST_0(PInst: PInstruction);
begin
  // FDIV  ST(4),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_4_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_5_ST_0(PInst: PInstruction);
begin
  // FDIV  ST(5),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_5_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_6_ST_0(PInst: PInstruction);
begin
  // FDIV  ST(6),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_6_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FDIV_ST_7_ST_0(PInst: PInstruction);
begin
  // FDIV  ST(7),ST(0)
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory := (INST_CATEGORY_ARITHMETIC or
    INST_CATEGORY_FLOATING_POINT or INST_CATEGORY_X87_FPU);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FDIV;
  Decode_ST_7_ST_0(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FDIV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXSAVE_M(PInst: PInstruction);
begin
  // FXSAVE  M
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory :=
    (INST_CATEGORY_SIMD or INST_CATEGORY_STATE_MANAGEMENT or
    INST_CATEGORY_X87_FPU or INST_CATEGORY__);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXSAVE;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXSAVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FPU_FXRSTOR_M(PInst: PInstruction);
begin
  // FXRSTOR  M
  PInst^.InstGroups := INST_GRP_FPU;
  PInst^.InstCategory :=
    (INST_CATEGORY_SIMD or INST_CATEGORY_STATE_MANAGEMENT or
    INST_CATEGORY_X87_FPU or INST_CATEGORY__);
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_FXRSTOR;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_FXRSTOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
