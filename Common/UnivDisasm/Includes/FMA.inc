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
// The Original Code is FMA.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_FMA_VFMADDSUB132PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADDSUB132PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUB132PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUB132PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADDSUB132PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADDSUB132PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUB132PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUB132PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUBADD132PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUBADD132PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADD132PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADD132PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUBADD132PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUBADD132PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADD132PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADD132PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD132PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADD132PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD132PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD132PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD132PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADD132PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD132PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD132PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD132SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFMADD132SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD132SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD132SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD132SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFMADD132SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD132SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD132SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB132PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUB132PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB132PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB132PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB132PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUB132PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB132PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB132PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB132SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFMSUB132SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB132SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB132SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB132SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFMSUB132SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB132SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB132SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD132PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMADD132PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD132PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD132PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD132PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMADD132PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD132PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD132PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD132SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFNMADD132SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD132SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD132SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD132SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFNMADD132SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD132SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD132SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB132PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMSUB132PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB132PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB132PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB132PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMSUB132PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB132PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB132PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB132SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFNMSUB132SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB132SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB132SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB132SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFNMSUB132SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB132SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB132SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADDSUB213PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADDSUB213PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUB213PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUB213PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADDSUB213PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADDSUB213PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUB213PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUB213PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUBADD213PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUBADD213PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADD213PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADD213PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUBADD213PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUBADD213PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADD213PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADD213PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD213PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADD213PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD213PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD213PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD213PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADD213PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD213PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD213PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD213SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFMADD213SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD213SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD213SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD213SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFMADD213SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD213SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD213SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB213PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUB213PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB213PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB213PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB213PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUB213PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB213PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB213PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB213SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFMSUB213SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB213SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB213SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB213SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFMSUB213SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB213SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB213SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD213PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMADD213PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD213PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD213PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD213PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMADD213PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD213PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD213PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD213SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFNMADD213SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD213SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD213SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD213SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFNMADD213SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD213SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD213SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB213PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMSUB213PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB213PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB213PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB213PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMSUB213PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB213PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB213PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB213SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFNMSUB213SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB213SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB213SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB213SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFNMSUB213SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB213SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB213SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADDSUB231PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADDSUB231PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUB231PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUB231PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADDSUB231PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADDSUB231PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADDSUB231PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADDSUB231PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUBADD231PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUBADD231PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADD231PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADD231PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUBADD231PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUBADD231PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUBADD231PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUBADD231PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD231PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADD231PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD231PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD231PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD231PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMADD231PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD231PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD231PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD231SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFMADD231SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD231SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD231SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMADD231SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFMADD231SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMADD231SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMADD231SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB231PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUB231PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB231PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB231PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB231PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFMSUB231PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB231PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB231PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB231SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFMSUB231SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB231SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB231SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFMSUB231SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFMSUB231SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFMSUB231SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFMSUB231SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD231PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMADD231PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD231PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD231PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD231PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMADD231PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD231PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD231PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD231SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFNMADD231SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD231SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD231SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMADD231SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFNMADD231SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMADD231SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMADD231SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB231PD_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMSUB231PD  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB231PD;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB231PD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB231PS_Vx_Hx_Wx(PInst: PInstruction);
begin
  // VFNMSUB231PS  Vx,Hx,Wx
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB231PS;
  Decode_Vx_Hx_Wx(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB231PS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB231SD_Vo_Ho_Wq(PInst: PInstruction);
begin
  // VFNMSUB231SD  Vo,Ho,Wq
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB231SD;
  Decode_Vo_Ho_Wq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB231SD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_FMA_VFNMSUB231SS_Vo_Ho_Wd(PInst: PInstruction);
begin
  // VFNMSUB231SS  Vo,Ho,Wd
  PInst^.InstGroups := INST_GRP_FMA;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VFNMSUB231SS;
  Decode_Vo_Ho_Wd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VFNMSUB231SS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
