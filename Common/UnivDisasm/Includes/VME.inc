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
// The Original Code is VME.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_VME_VMREAD_Ey_Gy(PInst: PInstruction);
begin
  // VMREAD  Ey,Gy
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMREAD;
  Decode_Ey_Gy(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMREAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMWRITE_Gy_Ey(PInst: PInstruction);
begin
  // VMWRITE  Gy,Ey
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMWRITE;
  Decode_Gy_Ey(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMWRITE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMCALL_void(PInst: PInstruction);
begin
  // VMCALL  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_VMCALL;
  DecodeBranch_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMCALL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMLAUNCH_void(PInst: PInstruction);
begin
  // VMLAUNCH  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMLAUNCH;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMLAUNCH);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMRESUME_void(PInst: PInstruction);
begin
  // VMRESUME  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMRESUME;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMRESUME);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMXOFF_void(PInst: PInstruction);
begin
  // VMXOFF  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMXOFF;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMXOFF);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMFUNC_void(PInst: PInstruction);
begin
  // VMFUNC  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMFUNC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMFUNC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMRUN_void(PInst: PInstruction);
begin
  // VMRUN  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMRUN;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMRUN);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMMCALL_void(PInst: PInstruction);
begin
  // VMMCALL  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.FlagsIndex := $00;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_VMMCALL;
  DecodeBranch_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMMCALL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMLOAD_void(PInst: PInstruction);
begin
  // VMLOAD  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMLOAD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMLOAD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMSAVE_void(PInst: PInstruction);
begin
  // VMSAVE  void
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMSAVE;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMSAVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMCLEAR_Mq(PInst: PInstruction);
begin
  // VMCLEAR  Mq
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefix66(PInst);
  PInst^.InstID := INST_ID_VMCLEAR;
  Decode_Mq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMCLEAR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMXON_Mq(PInst: PInstruction);
begin
  // VMXON  Mq
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_VMXON;
  Decode_Mq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMXON);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMPTRLD_Mq(PInst: PInstruction);
begin
  // VMPTRLD  Mq
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMPTRLD;
  Decode_Mq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMPTRLD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_VME_VMPTRST_Mq(PInst: PInstruction);
begin
  // VMPTRST  Mq
  PInst^.InstGroups := INST_GRP_VME;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VMPTRST;
  Decode_Mq(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VMPTRST);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
