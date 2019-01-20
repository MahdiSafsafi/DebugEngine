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
// The Original Code is CommonDecoders.inc
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

procedure Decode_word_integer(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_ST_0_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_1_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST1;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_2_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST2;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_3_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST3;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_4_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST4;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_5_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST5;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_6_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST6;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_7_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST7;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_void(PInst: PInstruction);
begin
  PInst^.nArg := $00;
end;

procedure Decode_single_real(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_ST_0_ST_1(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST1;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_0_ST_2(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST2;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_0_ST_3(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST3;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_0_ST_4(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST4;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_0_ST_5(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST5;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_0_ST_6(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST6;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_0_ST_7(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := REG_ST7;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_dword_integer(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_extended_real(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_TBYTE;
  DecodeArgAsMem(PInst, PInst^.Arg1);
end;

procedure Decode_M14_M28(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := Size14or28Sel[PInst^.Prefixes.OpSizePrf.Flags and PF_USED];
  DecodeArgAsMem(PInst, PInst^.Arg1);
end;

procedure Decode_Mw(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_Vo_Wo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Wo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vd_Wd(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_Vq_Wq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Vo_Wq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Wq_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_VDo_Vo_Wo(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_VDd_Vd_Wd(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_DWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_VDq_Vq_Wq(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Vn_K_z_Hn_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Ho_Wo(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vy_Hy_Wy(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_YWORD;
    PInst^.Arg3.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Pq_Qq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Vn_K_z_Hn_B32_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vx_Hx_Wx(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.LowerVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.LowerVL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.LowerVL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_Hn_B64_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vx_Wx(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.LowerVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.LowerVL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Wh_K_z_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.HalfVL;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.HalfVL;
    PInst^.Arg1.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Wf_K_z_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.FourthVL;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.FourthVL;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_We_K_z_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.EighthVL;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.EighthVL;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Vn_K_z_Wh_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.HalfVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.HalfVL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vy_Wo_o(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vu_K_z_Hu_B64_Wu(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.UpperVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.UpperVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.UpperVL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.UpperVL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vu_K_z_Hu_B32_Wu(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.UpperVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.UpperVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.UpperVL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.UpperVL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vy_Wy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_YWORD;
    PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_Wo_d(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vx_Mo_d(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vx_Uo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
end;

procedure Decode_Vu_K_z_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.UpperVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vy_Mo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vy_Uo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
end;

procedure Decode_Vu_K_z_Mo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.UpperVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_OWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vy_Mo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_OWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vz_K_z_My(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_ZWORD;
  PInst^.Arg1.Reg := (REGS_ZMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_YWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vn_K_z_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_B32_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_B64_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_Wh(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.HalfVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.HalfVL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_Wf(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.FourthVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.FourthVL;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Wo_d(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vy_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_We(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.EighthVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.EighthVL;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Wo_w(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_WORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vy_Wo_d(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_rK_K_Hn_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_rK_K_Hn_B64_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_rK_K_Hn_B32_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_mK(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Rm);
end;

procedure Decode_rK_Un(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
end;

procedure Decode_Vn_Mn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vo_Mo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_OWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vy_My(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_YWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vn_K_z_Hn_B64_Wn_er(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_Hn_B32_Wn_er(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vx_Hx_Mx(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.LowerVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_MEM;
  PInst^.Arg3.Size := PInst^.LowerVL;
  DecodeArgAsMem(PInst, PInst^.Arg3);
end;

procedure Decode_Vo_K_z_Ho_Wo_q_er(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_K_z_Ho_Wo_d_er(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Mx_Hx_Vx(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := PInst^.LowerVL;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.LowerVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := PInst^.LowerVL;
  PInst^.Arg3.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Vn_K_z_B64_Wn_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_B32_Wn_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_K_z_Ho_Wo_q_sae(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_K_z_Ho_Wo_d_sae(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_K_z_Ho_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_K_z_Ho_Wo_d(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vx_Mo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vn_K_z_Wo_b(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vx_Mo_b(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vn_K_z_Wo_w(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_WORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vx_Mo_w(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vn_K_z_Rb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
end;

procedure Decode_Vn_K_z_Rw(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_WORD;
  PInst^.Arg2.Reg := (REGS_GP or (SIZE_WORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Vn_K_z_Rq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Vn_K_z_Rd(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_DWORD;
  PInst^.Arg2.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Gy_Mo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_OWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Wn_K_z_M0_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.Fields.VL;
    PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_vsib_Vn_K_not_0_Mq_h(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.HalfVL);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_vsib_Vn_K_not_0_Md_n(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.Fields.VL);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_vsib_Vx_Mq_o_Hx(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, SIZE_OWORD);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := PInst^.LowerVL;
  PInst^.Arg3.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
end;

procedure Decode_vsib_Vx_Md_x_Hx(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.LowerVL);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := PInst^.LowerVL;
  PInst^.Arg3.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
end;

procedure Decode_vsib_Vn_K_not_0_Mq_n(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.Fields.VL);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_vsib_Vh_K_not_0_Md_n(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.Fields.VL);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := PInst^.HalfVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_vsib_Vx_Mq_x_Hx(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.LowerVL);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := PInst^.LowerVL;
  PInst^.Arg3.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
end;

procedure Decode_vsib_Vo_Md_x_Ho(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.LowerVL);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
end;

procedure Decode_Vo_Ho_Wq(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Ho_Wd(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_vsib_Mq_h_K_not_0_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.HalfVL);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_vsib_Md_n_K_not_0_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.Fields.VL);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_vsib_Mq_n_K_not_0_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.Fields.VL);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_vsib_Md_n_K_not_0_Vh(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, PInst^.Fields.VL);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.HalfVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Vn_Kw_z_Hn_B64_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vz_K_z_B64_Wz_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_ZWORD;
  PInst^.Arg1.Reg := (REGS_ZMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_ZWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_ZWORD;
    PInst^.Arg2.Reg := (REGS_ZMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vz_K_z_B32_Wz_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_ZWORD;
  PInst^.Arg1.Reg := (REGS_ZMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_ZWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_ZWORD;
    PInst^.Arg2.Reg := (REGS_ZMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gy_Eb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_BYTE;
    PInst^.Arg2.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
  end;
end;

procedure Decode_Gv_Mv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Gy_Ev(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Mv_Gv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
end;

procedure Decode_Gy_By_Ey(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.Fields.vvvv);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg3.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Gy_Ey_By(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg3.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.Fields.vvvv);
end;

procedure Decode_Gy_Ey(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Vo_Ho_Wo_Lo(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
  PInst^.Arg4.Size := SIZE_OWORD;
  PInst^.Arg4.Reg := (REGS_XMM or (Byte(PInst^.Arg4.Imm.Value and $F0) shr $04));
end;

procedure Decode_Vx_Hx_Lx_Wx(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.LowerVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
  PInst^.Arg3.Size := PInst^.LowerVL;
  PInst^.Arg3.Reg := (VL2Regs(PInst^.LowerVL) or (Byte(PInst^.Arg3.Imm.Value and $F0) shr $04));
  PInst^.Arg4.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg4.Size := PInst^.LowerVL;
    DecodeArgAsMem(PInst, PInst^.Arg4);
  end
  else
  begin
    PInst^.Arg4.Size := PInst^.LowerVL;
    PInst^.Arg4.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vx_Hx_Wx_Lx(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.LowerVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.LowerVL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.LowerVL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
  PInst^.Arg4.Size := PInst^.LowerVL;
  PInst^.Arg4.Reg := (VL2Regs(PInst^.LowerVL) or (Byte(PInst^.Arg4.Imm.Value and $F0) shr $04));
end;

procedure Decode_Vo_Ho_Lo_Wo(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or (Byte(PInst^.Arg3.Imm.Value and $F0) shr $04));
  PInst^.Arg4.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg4.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg4);
  end
  else
  begin
    PInst^.Arg4.Size := SIZE_OWORD;
    PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_VDo_VDo_Vo_Wo(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg4.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg4.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg4);
  end
  else
  begin
    PInst^.Arg4.Size := SIZE_OWORD;
    PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_VDo_VDo_Wo_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_OWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_VDd_VDd_Vd_Wd(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_DWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_DWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg4.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg4.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg4);
  end
  else
  begin
    PInst^.Arg4.Size := SIZE_DWORD;
    PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_VDd_VDd_Wd_Vd(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_DWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_DWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_VDq_VDq_Vq_Wq(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_QWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg4.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg4.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg4);
  end
  else
  begin
    PInst^.Arg4.Size := SIZE_QWORD;
    PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_VDq_VDq_Wq_Vq(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_QWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_VDo_Vo_Wo_VDo(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_OWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.Fields.Dest);
end;

procedure Decode_VDo_Wo_Vo_VDo(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_OWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.Fields.Dest);
end;

procedure Decode_VDd_Vd_Wd_VDd(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_DWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_DWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.Fields.Dest);
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_VDd_Wd_Vd_VDd(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_DWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_DWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.Fields.Dest);
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_VDq_Vq_Wq_VDq(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_QWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.Fields.Dest);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_VDq_Wq_Vq_VDq(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_QWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg4.Flags := AF_REG;
  PInst^.Arg4.Size := SIZE_QWORD;
  PInst^.Arg4.Reg := (REGS_XMM or PInst^.Fields.Dest);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_VDo_Wo_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.Dest);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_double_real(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Vo_Wo_Ho(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
end;

procedure Decode_Eb_Gb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    PInst^.Arg1.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Reg or PInst^.Fields.R];
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Ev_Gv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
end;

procedure Decode_Gb_Eb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Reg or PInst^.Fields.R];
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_BYTE;
    PInst^.Arg2.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
  end;
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Gv_Ev(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_AL_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_AL;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_rAX_Iz(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeZ;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeZ);
end;

procedure Decode_ES(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_SEG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := SEG_ES;
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_CS(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_SEG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := SEG_CS;
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_SS(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_SEG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := SEG_SS;
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_DS(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_SEG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := SEG_DS;
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_eAX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EAX);
end;

procedure Decode_eCX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_ECX);
end;

procedure Decode_eDX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EDX);
end;

procedure Decode_eBX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EBX);
end;

procedure Decode_eSP(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_ESP);
end;

procedure Decode_eBP(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EBP);
end;

procedure Decode_eSI(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_ESI);
end;

procedure Decode_eDI(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EDI);
end;

procedure Decode_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_r8(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R8);
end;

procedure Decode_rCX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RCX);
end;

procedure Decode_r9(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R9);
end;

procedure Decode_rDX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RDX);
end;

procedure Decode_r10(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R10);
end;

procedure Decode_rBX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RBX);
end;

procedure Decode_r11(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R11);
end;

procedure Decode_rSP(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RSP);
end;

procedure Decode_r12(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R12);
end;

procedure Decode_rBP(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RBP);
end;

procedure Decode_r13(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R13);
end;

procedure Decode_rSI(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RSI);
end;

procedure Decode_r14(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R14);
end;

procedure Decode_rDI(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RDI);
end;

procedure Decode_r15(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R15);
end;

procedure Decode_Gv_Ma(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeZ * 2;;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Gv_Ed(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    PInst^.Arg2.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Ew_Gw(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_WORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_WORD;
    PInst^.Arg1.Reg := (REGS_GP or (SIZE_WORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_WORD;
  PInst^.Arg2.Reg := (REGS_GP or (SIZE_WORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_Iz(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_IMM;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  DecodeArgAsImm(PInst, PInst^.Arg1, PInst^.InternalData.OpSizeZ);
end;

procedure Decode_Gv_Ev_Iz(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := PInst^.InternalData.OpSizeZ;
  DecodeArgAsImm(PInst, PInst^.Arg3, PInst^.InternalData.OpSizeZ);
end;

procedure Decode_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_IMM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg1, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Gv_Ev_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Yb_DX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsMemY(PInst, PInst^.Arg1, SIZE_BYTE);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_WORD;
  PInst^.Arg2.Reg := REG_DX;
end;

procedure Decode_Yz_DX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  DecodeArgAsMemY(PInst, PInst^.Arg1, PInst^.InternalData.OpSizeZ);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_WORD;
  PInst^.Arg2.Reg := REG_DX;
end;

procedure Decode_DX_Xb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := REG_DX;
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsMemX(PInst, PInst^.Arg2, SIZE_BYTE);
end;

procedure Decode_DX_Xz(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := REG_DX;
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeZ;
  DecodeArgAsMemX(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeZ);
end;

procedure Decode_Jb(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_RELATIVE_OFFSET;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeJ(PInst, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Mw_Sw(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_WORD;
  PInst^.Arg2.Reg := (REGS_SEG or PInst^.ModRm.Value.Reg);
  CheckOpSize(PInst, SIZE_WORD);
  CheckSeg(PInst);
end;

procedure Decode_Rv_Sw(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_WORD;
  PInst^.Arg2.Reg := (REGS_SEG or PInst^.ModRm.Value.Reg);
  CheckSeg(PInst);
end;

procedure Decode_Gv_M(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := 0;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Sw_Mw(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := (REGS_SEG or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  CheckOpSize(PInst, SIZE_WORD);
  CheckSeg(PInst);
end;

procedure Decode_Sw_Rv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := (REGS_SEG or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  CheckSeg(PInst);
end;

procedure Decode_rCX_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RCX);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_r9_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R9);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_rDX_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RDX);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_r10_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R10);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_rBX_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RBX);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_r11_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R11);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_rSP_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RSP);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_r12_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R12);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_rBP_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RBP);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_r13_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R13);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_rSI_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RSI);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_r14_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R14);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_rDI_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RDI);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_r15_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R15);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_AL_Ob(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_AL;
  PInst^.Arg2.Flags := AF_OFFSET;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsOffset(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_rAX_Ov(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
  PInst^.Arg2.Flags := AF_OFFSET;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsOffset(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_Ob_AL(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_OFFSET;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsOffset(PInst, PInst^.Arg1, SIZE_BYTE);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Reg := REG_AL;
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Ov_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_OFFSET;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsOffset(PInst, PInst^.Arg1, PInst^.InternalData.OpSizeV);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_Yb_Xb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsMemY(PInst, PInst^.Arg1, SIZE_BYTE);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsMemX(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Yv_Xv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsMemY(PInst, PInst^.Arg1, PInst^.InternalData.OpSizeV);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsMemX(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_Yb_AL(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsMemY(PInst, PInst^.Arg1, SIZE_BYTE);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Reg := REG_AL;
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Yv_rAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsMemY(PInst, PInst^.Arg1, PInst^.InternalData.OpSizeV);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
end;

procedure Decode_AL_Xb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_AL;
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsMemX(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_rAX_Xv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsMemX(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_R8B_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_R8B;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_CL_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_CL;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_R9B_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_R9B;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_DL_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_DL;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_R10B_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_R10B;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_BL_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_BL;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_R11B_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_R11B;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_AH_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := (PInst^.InternalData.MagicRex or REG_AH);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_R12B_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_R12B;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_CH_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := (PInst^.InternalData.MagicRex or REG_CH);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_R13B_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_R13B;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_DH_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := (PInst^.InternalData.MagicRex or REG_DH);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_R14B_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_R14B;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_BH_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := (PInst^.InternalData.MagicRex or REG_BH);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_R15B_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_R15B;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_rAX_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RAX);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_r8_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R8);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_rCX_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RCX);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_r9_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R9);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_rDX_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RDX);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_r10_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R10);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_rBX_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RBX);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_r11_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R11);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_rSP_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RSP);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_r12_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R12);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_rBP_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RBP);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_r13_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R13);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_rSI_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RSI);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_r14_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R14);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_rDI_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_RDI);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_r15_Iv(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or RID_R15);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeV);
end;

procedure Decode_Gv_Mp_w_v(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := (SIZE_WORD + PInst^.InternalData.OpSizeV);
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Iw_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_IMM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsImm(PInst, PInst^.Arg1, SIZE_WORD);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
end;

procedure Decode_eAX_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EAX);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
end;

procedure Decode_Ib_AL(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_IMM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg1, SIZE_BYTE);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Reg := REG_AL;
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Ib_eAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_IMM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg1, SIZE_BYTE);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EAX);
end;

procedure Decode_Jz(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_RELATIVE_OFFSET;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  DecodeJ(PInst, PInst^.InternalData.OpSizeZ);
end;

procedure Decode_AL_DX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_BYTE;
  PInst^.Arg1.Reg := REG_AL;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_WORD;
  PInst^.Arg2.Reg := REG_DX;
end;

procedure Decode_eAX_DX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EAX);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_WORD;
  PInst^.Arg2.Reg := REG_DX;
end;

procedure Decode_DX_AL(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := REG_DX;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Reg := REG_AL;
end;

procedure Decode_DX_eAX(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := REG_DX;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeZ shl 8) or RID_EAX);
end;

procedure Decode_packed_BCD(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_TBYTE;
  DecodeArgAsMem(PInst, PInst^.Arg1);
end;

procedure Decode_qword_integer(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_AX(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := REG_AX;
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_integer64(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_M98_M108(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := Size98or108Sel[PInst^.Prefixes.OpSizePrf.Flags and PF_USED];
  DecodeArgAsMem(PInst, PInst^.Arg1);
end;

procedure Decode_ST_0(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST0;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_1(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST1;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_2(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST2;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_3(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST3;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_4(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST4;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_5(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST5;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_6(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST6;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_ST_7(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := REG_ST7;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Gy_Ey_Id(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_DWORD;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_DWORD);
end;

procedure Decode_Gv_Ew(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_WORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_WORD;
    PInst^.Arg2.Reg := (REGS_GP or (SIZE_WORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Vo_K_z_Mo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vo_K_z_Ho_Uo(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
end;

procedure Decode_Vo_K_z_Mo_d(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vo_Mo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vo_Ho_Uo(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
end;

procedure Decode_Vo_Mo_d(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vo_Uo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
end;

procedure Decode_Mo_q_K_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Uo_K_z_Ho_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Mo_d_K_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Wn_K_z_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.Fields.VL;
    PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Wo_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Wy_Vy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_YWORD;
    PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Mo_q_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Uo_Ho_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Mo_d_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Uo_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Vo_Ho_Mo_q(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_MEM;
  PInst^.Arg3.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg3);
end;

procedure Decode_Vo_K_z_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vy_K_z_Wy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_YWORD;
    PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vz_K_z_Wz(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_ZWORD;
  PInst^.Arg1.Reg := (REGS_ZMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_ZWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_ZWORD;
    PInst^.Arg2.Reg := (REGS_ZMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Rd_CR8D(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_DWORD;
  PInst^.Arg2.Reg := REG_CR8D;
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_Ry_Cy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_CNTRL or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
end;

procedure Decode_Ry_Dy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_DBG or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
end;

procedure Decode_CR8D_Rd(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := REG_CR8D;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_DWORD;
  PInst^.Arg2.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_Cy_Ry(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_CNTRL or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Dy_Ry(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_DBG or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Ry_Ty(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_TAB or PInst^.ModRm.Value.Reg);
end;

procedure Decode_Ty_Ry(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_TAB or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Vo_Ho_Eq_er(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    PInst^.Arg3.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Vo_Ho_Ed_er(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    PInst^.Arg3.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Vo_Ho_Ey(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg3.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Vo_Mq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Vo_Nq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Vo_Ey(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Mn_Vn(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := PInst^.Fields.VL;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Mo_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_OWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_My_Vy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_YWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Mq_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Md_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Gq_Wo_q_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gq_Wo_d_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gd_Wo_q_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gd_Wo_d_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gy_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gy_Wo_d(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Pq_Wo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Pq_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gq_Wo_q_er(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gq_Wo_d_er(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gd_Wo_q_er(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Gd_Wo_d_er(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_DWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Wo_q_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Wo_d_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Ed(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_DWORD;
    PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_rK_vK_mK(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_QWORD;
  PInst^.Arg3.Reg := (REGS_MASK or PInst^.ModRm.Value.Rm);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_rK_mK(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Rm);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Gy_Uo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
end;

procedure Decode_Gy_Uy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
end;

procedure Decode_Vn_K_z_B64_Wn_er(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_B32_Wn_er(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Ho_Wo_q(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Ho_Wo_d(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vh_K_z_B64_Wn_er(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.HalfVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_B32_Wh_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.HalfVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.HalfVL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Wy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_YWORD;
    PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vy_Wo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_Hn_B64_Wn_sae(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vn_K_z_Hn_B32_Wn_sae(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Pq_Qd(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Vo_Eq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    PInst^.Arg2.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Vo_Ed(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    PInst^.Arg2.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Pq_Eq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    PInst^.Arg2.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Vn_K_z_B32_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vy_Wy_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_YWORD;
    PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Pq_Qq_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vh_K_z_B64_Wn_sae(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.HalfVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Uo_Ib_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Uo_Ib_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Ey_Gy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
end;

procedure Decode_Vn_K_z_B32_Wh_er(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_ROUNDING) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.HalfVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.HalfVL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Eq_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    PInst^.Arg1.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Ed_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_DWORD;
    PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Eq_Pq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    PInst^.Arg1.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Ed_Pq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_DWORD;
    PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
end;

procedure Decode_Qq_Pq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_rK_Md(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_rK_Mq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_rK_Mb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_rK_Mw(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_Eb(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    PInst^.Arg1.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
  end;
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Md_rK(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
end;

procedure Decode_Mq_rK(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Mb_rK(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
end;

procedure Decode_Mw_rK(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
end;

procedure Decode_rK_Rq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_rK_Ry(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Gq_mK(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Rm);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Gy_mK(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Rm);
end;

procedure Decode_FS(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_SEG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := SEG_FS;
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_Ev_Gv_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Ev_Gv_CL(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_BYTE;
  PInst^.Arg3.Reg := REG_CL;
end;

procedure Decode_GS(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  PInst^.Arg1.Flags := AF_SEG;
  PInst^.Arg1.Size := SIZE_WORD;
  PInst^.Arg1.Reg := SEG_GS;
  CheckOpSize(PInst, SIZE_WORD);
end;

procedure Decode_Gv_Eb(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_BYTE;
    PInst^.Arg2.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
  end;
end;

procedure Decode_rK_K_Hn_B64_Wn_sae(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_rK_K_Ho_Wo_q_sae(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_rK_K_Ho_Wo_d_sae(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_rK_K_Hn_B32_Wn_sae(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_My_Gy(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
end;

procedure Decode_Vo_Ho_Mw_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_MEM;
  PInst^.Arg3.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg3);
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Rv_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg3.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Mw_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Rv_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Pq_Mw_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Pq_Rv_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Gy_Uo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Gy_Nq_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_Hn_B64_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_Hn_B32_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Wo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vy_Hy_Wy_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_YWORD;
    PInst^.Arg3.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_Hn_Wo_b(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Wo_q_Vo(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
end;

procedure Decode_Pq_Uq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Gy_Nq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Mq_Pq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Vn_Hn_Wn(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Pq_Nq(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Reg or PInst^.Fields.R);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Vu_K_z_B64_Wu_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.UpperVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.UpperVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.UpperVL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vx_Hx_Wx_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.LowerVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.LowerVL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.LowerVL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vx_Wx_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.LowerVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.LowerVL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.LowerVL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.LowerVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_B64_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_B32_Wn_sae_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_B64_Wn_sae_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_K_z_Ho_Wo_d_sae_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Wo_d_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Wo_d_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_K_z_Ho_Wo_q_sae_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Wo_q_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Wo_q_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_Hn_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Mb_Vo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Ry_Vo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Mw_Vo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Eq_Vo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    PInst^.Arg1.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Ed_Vo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_DWORD;
    PInst^.Arg1.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Md_Vo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vu_K_z_Hu_Wo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.UpperVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.UpperVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vy_Hy_Wo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_OWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Wo_K_z_Vu_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.UpperVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Wo_Vy_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vz_K_z_Hz_Wy_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := SIZE_ZWORD;
  PInst^.Arg1.Reg := (REGS_ZMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_ZWORD;
  PInst^.Arg2.Reg := (REGS_ZMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_YWORD;
    PInst^.Arg3.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Wy_K_z_Vz_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_YWORD;
    PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_ZWORD;
  PInst^.Arg2.Reg := (REGS_ZMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Wh_K_z_Vn_sae_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.HalfVL;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.HalfVL;
    PInst^.Arg1.Reg := (VL2Regs(PInst^.HalfVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG or PInst^.SupportFlag(AF_SAE);
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Wo_q_Vo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Wo_o_Vy_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_OWORD;
    PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Mb_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_MEM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsMem(PInst, PInst^.Arg3);
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Rd_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_DWORD;
  PInst^.Arg3.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Mb_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Rd_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_DWORD;
  PInst^.Arg2.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Md_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_MEM;
  PInst^.Arg3.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg3);
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Uo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Md_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg2);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Uo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Eq_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    PInst^.Arg3.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Eq_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    PInst^.Arg2.Reg := (REGS_GP or (SIZE_QWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Ed_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    PInst^.Arg3.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vo_Ed_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    PInst^.Arg2.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vu_K_z_Hu_B64_Wu_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.UpperVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.UpperVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.UpperVL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.UpperVL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vu_K_z_Hu_B32_Wu_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.UpperVL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.UpperVL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.UpperVL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.UpperVL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.UpperVL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_rK_mK_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_QWORD;
  PInst^.Arg2.Reg := (REGS_MASK or PInst^.ModRm.Value.Rm);
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vy_Hy_Wy_Ly(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_YWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_YWORD;
    PInst^.Arg3.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
  PInst^.Arg4.Size := SIZE_YWORD;
  PInst^.Arg4.Reg := (REGS_YMM or (Byte(PInst^.Arg4.Imm.Value and $F0) shr $04));
end;

procedure Decode_Vn_K_z_Hn_B64_Wn_sae_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_Vn_K_z_Hn_B32_Wn_sae_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := PInst^.Fields.VL;
  PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_SAE) or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := PInst^.Fields.VL;
    PInst^.Arg3.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_IMM;
  PInst^.Arg4.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
end;

procedure Decode_rK_K_B64_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_rK_K_B32_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_rK_K_Wo_q_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_rK_K_Wo_d_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MASK or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_OWORD;
    PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Vo_Ho_Lo_Wd(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or (Byte(PInst^.Arg3.Imm.Value and $F0) shr $04));
  PInst^.Arg4.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg4.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg4);
  end
  else
  begin
    PInst^.Arg4.Size := SIZE_DWORD;
    PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Ho_Wd_Lo(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_DWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
  PInst^.Arg4.Size := SIZE_OWORD;
  PInst^.Arg4.Reg := (REGS_XMM or (Byte(PInst^.Arg4.Imm.Value and $F0) shr $04));
end;

procedure Decode_Vo_Ho_Lo_Wq(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
  PInst^.Arg3.Size := SIZE_OWORD;
  PInst^.Arg3.Reg := (REGS_XMM or (Byte(PInst^.Arg3.Imm.Value and $F0) shr $04));
  PInst^.Arg4.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg4.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg4);
  end
  else
  begin
    PInst^.Arg4.Size := SIZE_QWORD;
    PInst^.Arg4.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
end;

procedure Decode_Vo_Ho_Wq_Lo(PInst: PInstruction);
begin
  PInst^.nArg := $04;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Reg or PInst^.Fields.RRpExt());
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg3.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    DecodeArgAsMem(PInst, PInst^.Arg3);
  end
  else
  begin
    PInst^.Arg3.Size := SIZE_QWORD;
    PInst^.Arg3.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg4.Flags := AF_REG;
  DecodeArgAsImm(PInst, PInst^.Arg4, SIZE_BYTE);
  PInst^.Arg4.Size := SIZE_OWORD;
  PInst^.Arg4.Reg := (REGS_XMM or (Byte(PInst^.Arg4.Imm.Value and $F0) shr $04));
end;

procedure Decode_By_Ey_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.Fields.vvvv);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Eb_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    PInst^.Arg1.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
  end;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Ev_Iz(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeZ;
  DecodeArgAsImm(PInst, PInst^.Arg2, PInst^.InternalData.OpSizeZ);
end;

procedure Decode_Ev_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
end;

procedure Decode_Uo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
end;

procedure Decode_Nq_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_QWORD;
  PInst^.Arg1.Reg := (REGS_MMX or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  PInst^.Arg2.Flags := AF_IMM;
  PInst^.Arg2.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg2, SIZE_BYTE);
end;

procedure Decode_Ho_Uo_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_XMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_XMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Hy_Uy_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_YWORD;
  PInst^.Arg1.Reg := (REGS_YMM or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_YWORD;
  PInst^.Arg2.Reg := (REGS_YMM or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Hn_K_z_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Hn_K_z_B32_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST32);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Hn_K_z_B64_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG or PInst^.SupportFlag(AF_MASK) or PInst^.SupportFlag(AF_ZEROING);
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem] or PInst^.SupportFlag(AF_BROADCAST64);
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Hn_Wn_Ib(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.Fields.VL;
  PInst^.Arg1.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.Fields.vvvv or PInst^.Fields.Vp);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.Fields.VL;
    PInst^.Arg2.Reg := (VL2Regs(PInst^.Fields.VL) or PInst^.ModRm.Value.Rm or PInst^.Fields.XBExt());
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_BYTE;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_BYTE);
end;

procedure Decode_Ry(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_M(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := 0;
  DecodeArgAsMem(PInst, PInst^.Arg1);
end;

procedure Decode_Md(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_Ev(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_rB_Mib(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_BND or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := 0;
  DecodeArgAsMib(PInst, PInst^.Arg2);
end;

procedure Decode_rB_mB(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_BND or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_BND or PInst^.ModRm.Value.Rm);
end;

procedure Decode_rB_M(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_BND or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := 0;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_rB_Ey(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_BND or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_Mib_rB(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := 0;
  DecodeArgAsMib(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_BND or PInst^.ModRm.Value.Reg);
end;

procedure Decode_mB_rB(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_BND or PInst^.ModRm.Value.Rm);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_BND or PInst^.ModRm.Value.Reg);
end;

procedure Decode_M_rB(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := 0;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_OWORD;
  PInst^.Arg2.Reg := (REGS_BND or PInst^.ModRm.Value.Reg);
end;

procedure Decode_rB_My(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := SIZE_OWORD;
  PInst^.Arg1.Reg := (REGS_BND or PInst^.ModRm.Value.Reg);
  PInst^.Arg2.Flags := AF_MEM;
  PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
  DecodeArgAsMem(PInst, PInst^.Arg2);
end;

procedure Decode_By_Ey(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.Fields.vvvv);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := PInst^.InternalData.OpSizeY;
    PInst^.Arg2.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
end;

procedure Decode_vsib_Md_z_K_not_0(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, SIZE_ZWORD);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_DWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_DWORD);
end;

procedure Decode_vsib_Mq_y_K_not_0(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, SIZE_YWORD);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_vsib_Mq_z_K_not_0(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  DecodeVSIB(PInst, SIZE_ZWORD);
  PInst^.Arg1.Flags := AF_MEM or PInst^.SupportFlag(AF_MASK);
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Eb_1(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    PInst^.Arg1.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
  end;
  PInst^.Arg2.Flags := AF_IMM or AF_EMB;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Imm.Size := SIZE_BYTE;
  PInst^.Arg2.Imm.Value := 1;
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Ev_1(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_IMM or AF_EMB;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Imm.Size := SIZE_BYTE;
  PInst^.Arg2.Imm.Value := 1;
end;

procedure Decode_Eb_CL(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := SIZE_BYTE;
    PInst^.Arg1.Reg := Reg8RexMask[PInst^.Prefixes.Rex.Flags and PF_USED][PInst^.ModRm.Value.Rm or PInst^.Fields.B];
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Reg := REG_CL;
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_Ev_CL(PInst: PInstruction);
begin
  PInst^.nArg := $02;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end
  else
  begin
    PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg2.Flags := AF_REG;
  PInst^.Arg2.Size := SIZE_BYTE;
  PInst^.Arg2.Reg := REG_CL;
end;

procedure Decode_Rv(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
end;

procedure Decode_Mp_w_y(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := (SIZE_WORD + PInst^.InternalData.OpSizeY);
  DecodeArgAsMem(PInst, PInst^.Arg1);
end;

procedure Decode_Mq(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_QWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_QWORD);
end;

procedure Decode_Mo(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_OWORD;
  DecodeArgAsMem(PInst, PInst^.Arg1);
end;

procedure Decode_Mb(PInst: PInstruction);
begin
  PInst^.nArg := $01;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := SIZE_BYTE;
  DecodeArgAsMem(PInst, PInst^.Arg1);
  CheckOpSize(PInst, SIZE_BYTE);
end;

procedure Decode_By_Ed_Id(PInst: PInstruction);
begin
  PInst^.nArg := $03;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := AF_REG;
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeY;
  PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeY shl 8) or PInst^.Fields.vvvv);
  PInst^.Arg2.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  if PInst^.ModRm.Value.IsMem then
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    DecodeArgAsMem(PInst, PInst^.Arg2);
  end
  else
  begin
    PInst^.Arg2.Size := SIZE_DWORD;
    PInst^.Arg2.Reg := (REGS_GP or (SIZE_DWORD shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  PInst^.Arg3.Flags := AF_IMM;
  PInst^.Arg3.Size := SIZE_DWORD;
  DecodeArgAsImm(PInst, PInst^.Arg3, SIZE_DWORD);
end;
