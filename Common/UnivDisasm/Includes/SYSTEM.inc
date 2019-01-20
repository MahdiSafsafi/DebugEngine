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
// The Original Code is SYSTEM.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_SYSTEM_ARPL_Ew_Gw(PInst: PInstruction);
begin
  // ARPL  Ew,Gw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $08;
  PInst^.InstID := INST_ID_ARPL;
  Decode_Ew_Gw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_ARPL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_HLT_void(PInst: PInstruction);
begin
  // HLT  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $25;
  PInst^.InstID := INST_ID_HLT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_HLT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LAR_Gv_Ew(PInst: PInstruction);
begin
  // LAR  Gv,Ew
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $36;
  PInst^.InstID := INST_ID_LAR;
  Decode_Gv_Ew(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LAR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LSL_Gv_Ew(PInst: PInstruction);
begin
  // LSL  Gv,Ew
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $3F;
  PInst^.InstID := INST_ID_LSL;
  Decode_Gv_Ew(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LSL);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_CLTS_void(PInst: PInstruction);
begin
  // CLTS  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $12;
  PInst^.InstID := INST_ID_CLTS;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CLTS);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_INVD_void(PInst: PInstruction);
begin
  // INVD  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $2D;
  PInst^.InstID := INST_ID_INVD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_WBINVD_void(PInst: PInstruction);
begin
  // WBINVD  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $6B;
  PInst^.InstID := INST_ID_WBINVD;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_WBINVD);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_WRMSR_void(PInst: PInstruction);
begin
  // WRMSR  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $6C;
  PInst^.InstID := INST_ID_WRMSR;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_WRMSR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_RDTSC_void(PInst: PInstruction);
begin
  // RDTSC  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $53;
  PInst^.InstID := INST_ID_RDTSC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RDTSC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_RDMSR_void(PInst: PInstruction);
begin
  // RDMSR  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $51;
  PInst^.InstID := INST_ID_RDMSR;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RDMSR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_RDPMC_void(PInst: PInstruction);
begin
  // RDPMC  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $52;
  PInst^.InstID := INST_ID_RDPMC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RDPMC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_SYSENTER_void(PInst: PInstruction);
begin
  // SYSENTER  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SYSENTER;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SYSENTER);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_SYSEXIT_void(PInst: PInstruction);
begin
  // SYSEXIT  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_SYSEXIT;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SYSEXIT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_JMPE_Jz(PInst: PInstruction);
begin
  // JMPE  Jz
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JMPE;
  Decode_Jz(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JMPE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_RDFSBASE_Ry(PInst: PInstruction);
begin
  // RDFSBASE  Ry
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_RDFSBASE;
  Decode_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RDFSBASE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_RDGSBASE_Ry(PInst: PInstruction);
begin
  // RDGSBASE  Ry
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_RDGSBASE;
  Decode_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RDGSBASE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_WRFSBASE_Ry(PInst: PInstruction);
begin
  // WRFSBASE  Ry
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_WRFSBASE;
  Decode_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_WRFSBASE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_WRGSBASE_Ry(PInst: PInstruction);
begin
  // WRGSBASE  Ry
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  MakeMndPrefixF3(PInst);
  PInst^.InstID := INST_ID_WRGSBASE;
  Decode_Ry(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_WRGSBASE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_XSAVE_M(PInst: PInstruction);
begin
  // XSAVE  M
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XSAVE;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XSAVE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_XRSTOR_M(PInst: PInstruction);
begin
  // XRSTOR  M
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XRSTOR;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XRSTOR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_XSAVEOPT_M(PInst: PInstruction);
begin
  // XSAVEOPT  M
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XSAVEOPT;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XSAVEOPT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_SLDT_Mw(PInst: PInstruction);
begin
  // SLDT  Mw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $5F;
  PInst^.InstID := INST_ID_SLDT;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SLDT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_SLDT_Rv(PInst: PInstruction);
begin
  // SLDT  Rv
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $5F;
  PInst^.InstID := INST_ID_SLDT;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SLDT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_STR_Mw(PInst: PInstruction);
begin
  // STR  Mw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $65;
  PInst^.InstID := INST_ID_STR;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_STR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_STR_Rv(PInst: PInstruction);
begin
  // STR  Rv
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $65;
  PInst^.InstID := INST_ID_STR;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_STR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LLDT_Mw(PInst: PInstruction);
begin
  // LLDT  Mw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $3A;
  PInst^.InstID := INST_ID_LLDT;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LLDT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LLDT_Rv(PInst: PInstruction);
begin
  // LLDT  Rv
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $3A;
  PInst^.InstID := INST_ID_LLDT;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LLDT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LTR_Mw(PInst: PInstruction);
begin
  // LTR  Mw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $40;
  PInst^.InstID := INST_ID_LTR;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LTR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LTR_Rv(PInst: PInstruction);
begin
  // LTR  Rv
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $40;
  PInst^.InstID := INST_ID_LTR;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LTR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_VERR_Mw(PInst: PInstruction);
begin
  // VERR  Mw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $69;
  PInst^.InstID := INST_ID_VERR;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VERR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_VERR_Rv(PInst: PInstruction);
begin
  // VERR  Rv
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $69;
  PInst^.InstID := INST_ID_VERR;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VERR);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_VERW_Mw(PInst: PInstruction);
begin
  // VERW  Mw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VERW;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VERW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_VERW_Rv(PInst: PInstruction);
begin
  // VERW  Rv
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_VERW;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_VERW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_JMPE_Ev(PInst: PInstruction);
begin
  // JMPE  Ev
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.DstAddr.Flags := JF_BRANCH;
  PInst^.InstID := INST_ID_JMPE;
  DecodeBranch_Ev(PInst);
  ValidateBnd(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_JMPE);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_CLAC_void(PInst: PInstruction);
begin
  // CLAC  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_CLAC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_CLAC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_STAC_void(PInst: PInstruction);
begin
  // STAC  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_STAC;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_STAC);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_XGETBV_void(PInst: PInstruction);
begin
  // XGETBV  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XGETBV;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XGETBV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_XSETBV_void(PInst: PInstruction);
begin
  // XSETBV  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_XSETBV;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_XSETBV);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_RDTSCP_void(PInst: PInstruction);
begin
  // RDTSCP  void
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $00;
  PInst^.InstID := INST_ID_RDTSCP;
  Decode_void(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_RDTSCP);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_SGDT_Mp_w_y_F64(PInst: PInstruction);
begin
  // SGDT  Mp(w:y)
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $5F;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_SGDT;
  Decode_Mp_w_y(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SGDT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_SIDT_Mp_w_y_F64(PInst: PInstruction);
begin
  // SIDT  Mp(w:y)
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $5F;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_SIDT;
  Decode_Mp_w_y(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SIDT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LGDT_Mp_w_y_F64(PInst: PInstruction);
begin
  // LGDT  Mp(w:y)
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $3A;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_LGDT;
  Decode_Mp_w_y(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LGDT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LIDT_Mp_w_y_F64(PInst: PInstruction);
begin
  // LIDT  Mp(w:y)
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $3A;
  PInst^.SetSizeToF64();
  PInst^.InstID := INST_ID_LIDT;
  Decode_Mp_w_y(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LIDT);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_SMSW_Mw(PInst: PInstruction);
begin
  // SMSW  Mw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $5F;
  PInst^.InstID := INST_ID_SMSW;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SMSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_SMSW_Rv(PInst: PInstruction);
begin
  // SMSW  Rv
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_STORE;
  PInst^.FlagsIndex := $5F;
  PInst^.InstID := INST_ID_SMSW;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_SMSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LMSW_Mw(PInst: PInstruction);
begin
  // LMSW  Mw
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $3A;
  PInst^.InstID := INST_ID_LMSW;
  Decode_Mw(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LMSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_LMSW_Rv(PInst: PInstruction);
begin
  // LMSW  Rv
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_LOAD;
  PInst^.FlagsIndex := $3A;
  PInst^.InstID := INST_ID_LMSW;
  Decode_Rv(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_LMSW);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;

procedure Decode_SYSTEM_INVLPG_M(PInst: PInstruction);
begin
  // INVLPG  M
  PInst^.InstGroups := INST_GRP_SYSTEM;
  PInst^.InstCategory := INST_CATEGORY_NIL;
  PInst^.FlagsIndex := $2E;
  PInst^.InstID := INST_ID_INVLPG;
  Decode_M(PInst);
{$IFDEF NEED_DISPLAY}
  MoveMnem(PInst, MNEM_INVLPG);
  SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
end;
