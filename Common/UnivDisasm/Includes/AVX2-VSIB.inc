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
// The Original Code is AVX2-VSIB.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_AVX2_VSIB_VPGATHERDQ_Vx_Mq_o_Hx(PInst: PInstruction);
begin
  // VPGATHERDQ  Vx,Mq.o,Hx
  PInst^.InstGroups := (INST_GRP_AVX2 or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPGATHERDQ;
  Decode_vsib_Vx_Mq_o_Hx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPGATHERDQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VSIB_VPGATHERDD_Vx_Md_x_Hx(PInst: PInstruction);
begin
  // VPGATHERDD  Vx,Md.x,Hx
  PInst^.InstGroups := (INST_GRP_AVX2 or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPGATHERDD;
  Decode_vsib_Vx_Md_x_Hx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPGATHERDD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VSIB_VPGATHERQQ_Vx_Mq_x_Hx(PInst: PInstruction);
begin
  // VPGATHERQQ  Vx,Mq.x,Hx
  PInst^.InstGroups := (INST_GRP_AVX2 or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPGATHERQQ;
  Decode_vsib_Vx_Mq_x_Hx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPGATHERQQ);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VSIB_VPGATHERQD_Vo_Md_x_Ho(PInst: PInstruction);
begin
  // VPGATHERQD  Vo,Md.x,Ho
  PInst^.InstGroups := (INST_GRP_AVX2 or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VPGATHERQD;
  Decode_vsib_Vo_Md_x_Ho(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VPGATHERQD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VSIB_VGATHERDPD_Vx_Mq_o_Hx(PInst: PInstruction);
begin
  // VGATHERDPD  Vx,Mq.o,Hx
  PInst^.InstGroups := (INST_GRP_AVX2 or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VGATHERDPD;
  Decode_vsib_Vx_Mq_o_Hx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERDPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VSIB_VGATHERDPS_Vx_Md_x_Hx(PInst: PInstruction);
begin
  // VGATHERDPS  Vx,Md.x,Hx
  PInst^.InstGroups := (INST_GRP_AVX2 or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VGATHERDPS;
  Decode_vsib_Vx_Md_x_Hx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERDPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VSIB_VGATHERQPD_Vx_Mq_x_Hx(PInst: PInstruction);
begin
  // VGATHERQPD  Vx,Mq.x,Hx
  PInst^.InstGroups := (INST_GRP_AVX2 or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VGATHERQPD;
  Decode_vsib_Vx_Mq_x_Hx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERQPD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_AVX2_VSIB_VGATHERQPS_Vo_Md_x_Ho(PInst: PInstruction);
begin
  // VGATHERQPS  Vo,Md.x,Ho
  PInst^.InstGroups := (INST_GRP_AVX2 or INST_GRP_VSIB);
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.SetSp(SP_DISP8_VE_64);
  PInst^.InstID := INST_ID_VGATHERQPS;
  Decode_vsib_Vo_Md_x_Ho(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VGATHERQPS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
