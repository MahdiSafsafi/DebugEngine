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
// The Original Code is UnivDisasm.Internal.Common.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Internal.Common;

interface

{$I Config.inc}

uses
  UnivDisasm.Disasm,
  UnivDisasm.Cnsts,
  UnivDisasm.Cnsts.Instructions;

function GetDispN(PInst: PInstruction; InputSize: UInt8): UInt8;

procedure DecodeModRm(PInst: PInstruction);
procedure DecodeVSIB(PInst: PInstruction; VsibSize: Byte);
procedure DecodeArgAsMib(PInst: PInstruction; var Arg: TArgument);

procedure DecodeJ(PInst: PInstruction; Size: Byte);
procedure DecodeBranch_void(PInst: PInstruction);
procedure DecodeBranch_Ev(PInst: PInstruction);
procedure DecodeBranch_Ap_w_z(PInst: PInstruction);
procedure DecodeBranch_Mp_w_z(PInst: PInstruction);
procedure DecodeBranch_Iw(PInst: PInstruction);

procedure DecodeArgAsMem(PInst: PInstruction; var Arg: TArgument);
procedure DecodeArgAsMemX(PInst: PInstruction; var Arg: TArgument; Size: Byte);
procedure DecodeArgAsMemY(PInst: PInstruction; var Arg: TArgument; Size: Byte);

procedure DecodeArgAsOffset(PInst: PInstruction; var Arg: TArgument; Size: Byte);

procedure DecodeArgAsImm(PInst: PInstruction; var Arg: TArgument; Size: Byte);
procedure DecodeImmAsOpExt(PInst: PInstruction);

procedure ValidateBnd(PInst: PInstruction);
procedure ValidateLock(PInst: PInstruction);
procedure ValidateXRelease(PInst: PInstruction);
procedure ValidateRep(PInst: PInstruction);
procedure ValidateRepe(PInst: PInstruction);
procedure ValidateRepne(PInst: PInstruction);

procedure CheckOpSize(PInst: PInstruction; Size: Byte); {$IFDEF MustInline} inline; {$ENDIF}
procedure CheckSeg(PInst: PInstruction); {$IFDEF MustInline} inline; {$ENDIF}
procedure SuperfluousPrefix(PInst: PInstruction; var P: TPrefix); {$IFDEF MustInline} inline; {$ENDIF}
procedure MakeMndPrefix66(PInst: PInstruction); {$IFDEF MustInline} inline; {$ENDIF}
procedure MakeMndPrefixF0(PInst: PInstruction); {$IFDEF MustInline} inline; {$ENDIF}
procedure MakeMndPrefixF2(PInst: PInstruction); {$IFDEF MustInline} inline; {$ENDIF}
procedure MakeMndPrefixF3(PInst: PInstruction); {$IFDEF MustInline} inline; {$ENDIF}
function VL2Regs(VLSize: Integer): TReg; {$IFDEF MustInline} inline; {$ENDIF}
procedure Decode_INVALID(PInst: PInstruction);

{$I ModRmFlags.inc}

const
  MemRegFlagsSel: array [Boolean] of Byte = (AF_REG, AF_MEM);

implementation

{$HINTS Off}

uses
  UnivDisasm.Internal.Prefixes,
  UnivDisasm.Cnsts.Regs;

const
  AMToSize: array [0 .. 3] of Byte = ( //
    0, { ?? }
    SIZE_WORD, { AM16 }
    SIZE_DWORD, { AM32 }
    SIZE_QWORD { AM64 } );

  { Utils }
function SegOrDef(PInst: PInstruction; Default: TReg): TReg;
begin
  if PInst^.InternalData.Seg > 0 then
    Exit(PInst^.InternalData.Seg)
  else
  begin
    case Default of
      SEG_FS, SEG_GS: Exit(Default);
    end;
    if PInst^.Arch <> CPUX64 then
      Exit(Default);
  end;
  Result := 0;
end;

{ Disp8 * N Routines }
function GetDispSizeN(Disp: Int32): UInt8; {$IFDEF MustInline} inline; {$ENDIF}
const
  SizeSel: array [Boolean] of UInt8 = (SIZE_1_BYTE, SIZE_2_BYTE);
begin
  { Since 0xFF * 64 = 0x3FC0 => disp8*n size is guaranteed to be 1 or 2 bytes }
  Result := SizeSel[Disp > $FF];
end;

function GetDispN(PInst: PInstruction; InputSize: UInt8): UInt8;
const
  FV2N: array [Boolean] of array [Boolean] of array [0 .. 3] of UInt8 = ( //
    ((SIZE_16_BYTE, SIZE_32_BYTE, SIZE_64_BYTE, 0) { evex.w0.bc0 } , //
    (SIZE_4_BYTE, SIZE_4_BYTE, SIZE_4_BYTE, 0) { evex.w0.bc1 } ), //
    ((SIZE_16_BYTE, SIZE_32_BYTE, SIZE_64_BYTE, 0) { evex.w1.bc0 } , //
    (SIZE_8_BYTE, SIZE_8_BYTE, SIZE_8_BYTE, 0)) { evex.w1.bc1 }
    );
  HV2N: array [Boolean] of array [0 .. 3] of UInt8 = ( //
    (SIZE_8_BYTE, SIZE_16_BYTE, SIZE_32_BYTE, 0), { evex.bc0 }
    (SIZE_4_BYTE, SIZE_4_BYTE, SIZE_4_BYTE, 0) { evex.bc1 }
    );
  DUP2N: array [0 .. 3] of UInt8 = ( //
    SIZE_8_BYTE, { VL128 }
    SIZE_32_BYTE, { VL256 }
    SIZE_64_BYTE, { VL512 }
    0 { VL??? } //
    );
  W2Size: array [Boolean] of UInt8 = (SIZE_DWORD, SIZE_QWORD);
var
  Tuple, N: UInt8;
  bc, W: Boolean;
  VL, VLSize: UInt8;
  ins: Word;
begin
  N := 0;
  W := PInst^.Fields.W;
  bc := PInst^.Fields.bc;
  VLSize := PInst^.Fields.VL;
  if VLSize > 0 then
    VL := VLSize shr 5
  else
    VL := 3;
  ins := PInst^.InstID;
  Tuple := PInst^.InternalData.Tuple;

  case Tuple of
    TT_FV: N := FV2N[W][bc][VL];
    TT_HV: N := HV2N[bc][VL];
    TT_FVM: N := VLSize;
    TT_T1S:
      begin
        { Instructions marked with SP_DISP8_VE must be
          handled first, because those instructions doesn't
          follow the traditional T1S disp8*N rules.
          ==> Special disp8 *N form !
        }
        if PInst^.InternalData.Sp and SP_DISP8_VE_32 = SP_DISP8_VE_32 then
          N := SIZE_4_BYTE
        else if PInst^.InternalData.Sp and SP_DISP8_VE_64 = SP_DISP8_VE_64 then
          N := SIZE_8_BYTE
        else
          case InputSize of
            SIZE_1_BYTE: N := SIZE_1_BYTE;
            SIZE_2_BYTE: N := SIZE_2_BYTE;
            SIZE_4_BYTE: N := SIZE_4_BYTE;
            SIZE_8_BYTE: N := SIZE_8_BYTE;
          else N := W2Size[W];
          end;
      end;
    TT_T1F:
      begin
        if InputSize = SIZE_4_BYTE then
          N := SIZE_4_BYTE
        else if InputSize = SIZE_8_BYTE then
          N := SIZE_8_BYTE;
      end;
    TT_T2:
      begin
        if W then
        begin
          if VLSize > VL128 then
            N := SIZE_16_BYTE;
        end
        else
          N := SIZE_8_BYTE;
      end;
    TT_T4:
      begin
        if W then
        begin
          if VLSize = VL512 then
            N := SIZE_32_BYTE;
        end else begin
          if VLSize > VL128 then
            N := SIZE_16_BYTE;
        end;
      end;
    TT_T8:
      begin
        if (not W) and (VLSize = VL512) then
          N := SIZE_32_BYTE;
      end;
    TT_HVM: N := VLSize shr 1;
    TT_QVM: N := VLSize shr 2;
    TT_OVM: N := VLSize shr 3;
    TT_M128: N := SIZE_16_BYTE;
    TT_DUP: N := DUP2N[VL];
  end;
  Result := N;
end;

{ ModRm & Sib Routine }

procedure DecodeModRm(PInst: PInstruction);
const
  FlagsSizeToDispSize: array [0 .. 3] of Byte = ( //
    0, // ??
    SIZE_BYTE, // Byte
    SIZE_WORD, // Word
    SIZE_DWORD // Dword
    );
var
  F: Byte;
  DispSize: Byte;
begin
  if PInst^.ModRm.Flags and MF_USED <> 0 then
    Exit;
  PInst^.ModRm.Value := PInst^.NextInst^;
  F := AM2ModRmFlags[PInst^.AddressMode]^[PInst^.ModRm.Value];
  if not PInst^.Next() then
    Exit;
  if F and MF_SIB <> 0 then
  begin
    PInst^.Sib.Value := PInst^.NextInst^;
    PInst^.Sib.Flags := SF_USED;
    if not PInst^.Next() then
      Exit;
  end;
  if PInst^.InternalData.DRex then
    Decode_PREFIXES_DREX_void(PInst);

  DispSize := FlagsSizeToDispSize[(F and MF_SIZE_MASK) shr 4];
  if (DispSize = 0) and (PInst^.Sib.Value.Base = $05) then
  begin
    if PInst^.ModRm.Value.&Mod = $00 then
      DispSize := SIZE_DWORD
    else
    begin
      DispSize := SIZE_BYTE;
    end;
  end;
  if DispSize > 0 then
  begin
    PInst^.Disp.Flags := DF_USED;
    if (F and MF_EXTRA_MASK = MF_N1) and (PInst^.Arch = CPUX64) then
    begin
      if (PInst^.AddressMode = AM_64) or (PInst^.AddressMode = AM_32) then
        PInst^.Disp.Flags := DF_USED or DF_REL;
    end;
    PInst^.Disp.Size := DispSize;
    case DispSize of
      SIZE_BYTE: PInst^.Disp.Value := PInt8(PInst^.NextInst)^;
      SIZE_WORD: PInst^.Disp.Value := PInt16(PInst^.NextInst)^;
      SIZE_DWORD: PInst^.Disp.Value := PInt32(PInst^.NextInst)^;
    end;
    PInst^.Next(DispSize);
  end;
  PInst^.ModRm.Flags := F;
end;

procedure DecodeVSIB(PInst: PInstruction; VsibSize: Byte);
var
  Reg: TReg;
begin
  { VSIB }
  PInst^.Sib.Flags := PInst^.Sib.Flags or SF_VSIB;
  Reg := REGS_XMM;
  case VsibSize of
    SIZE_YWORD: Reg := REGS_YMM;
    SIZE_ZWORD: Reg := REGS_ZMM;
  end;
  Reg := Reg or (PInst^.Sib.Value.Index) or (PInst^.Fields.X) or (PInst^.Fields.Vp);
  PInst^.InternalData.VSibReg := Reg;
end;

procedure DecodeArgAsMib(PInst: PInstruction; var Arg: TArgument);
begin
  DecodeArgAsMem(PInst, Arg);
  if PInst^.Sib.Flags and SF_USED = 0 then
    PInst^.Error(ERROR_SIB_EXPECTED)
  else
    PInst^.Sib.Flags := PInst^.Sib.Flags or SF_MIB;
  if PInst^.Disp.Flags and DF_REL <> 00 then
    PInst^.Error(ERROR_RELATIVE_ADDRESS_PROHIBITED);
  if Arg.Mem.IndexReg > 0 then
    PInst^.Warn(WARN_INDEX_REG_NOT_USED_IN_EAC);
end;

{ Brnach & Rel offset }
procedure DecodeJ(PInst: PInstruction; Size: Byte);
var
  F: Word;
  T: Byte;
  VA, P: PByte;
  Value: Int64;
begin
  Value := 0;
  F := PInst^.DstAddr.Flags or JF_USED or JF_REL;;
  case Size of
    SIZE_BYTE: Value := PInt8(PInst^.NextInst)^;
    SIZE_WORD: Value := PInt16(PInst^.NextInst)^;
    SIZE_DWORD: Value := PInt32(PInst^.NextInst)^;
    SIZE_QWORD: Value := PInt64(PInst^.NextInst)^;
  else
    begin
      PInst^.Error(ERROR_INVALID_OPERAND_SIZE);
      { Keep process anyway }
    end;
  end;
  if not PInst^.Next(Size) then
    Exit;
  PInst^.Arg1.Imm.Value := Value;
  if Assigned(PInst^.VirtualAddr) then
    VA := PInst^.VirtualAddr + (PInst^.NextInst - PInst^.Addr)
  else
    VA := PInst^.NextInst;
  P := VA + Value;
  try
    if PInst^.Options and GO_TEST_ADDRESS <> 0 then
      T := P^;
  except
    PInst^.Error(ERROR_INVALID_EFFECTIVE_ADDRESS);
  end;
  PInst^.DstAddr.Addr := P;
  if Size = SIZE_BYTE then
    F := F or JF_SHORT
  else
    F := F or JF_NEAR;
  PInst^.DstAddr.Flags := F;
end;

procedure DecodeBranch_void(PInst: PInstruction);
begin
  PInst^.DstAddr.Flags := PInst^.DstAddr.Flags or JF_USED or JF_NEAR;
end;

procedure DecodeBranch_Ev(PInst: PInstruction);
{ TODO: Remove DS and compute address when segment is not DS. }
var
  VA, P: PByte;
  F: Word;
  DS: Word;
begin
  F := PInst^.DstAddr.Flags or JF_USED or JF_ABSOLUTE or JF_INDIRECT or JF_NEAR;
  DecodeModRm(PInst);
  PInst^.Arg1.Flags := MemRegFlagsSel[PInst^.ModRm.Value.IsMem];
  PInst^.Arg1.Size := PInst^.InternalData.OpSizeV;
  if PInst^.ModRm.Value.IsMem then
  begin
    DecodeArgAsMem(PInst, PInst^.Arg1);
  end else begin
    PInst^.Arg1.Reg := (REGS_GP or (PInst^.InternalData.OpSizeV shl 8) or PInst^.ModRm.Value.Rm or PInst^.Fields.B);
  end;
  DS := SegOrDef(PInst, SEG_DS);
{$IFDEF CPUX64}
  if DS = 0 then
    DS := SEG_DS;
{$ENDIF CPUX64}
  if Assigned(PInst^.VirtualAddr) then
    VA := PInst^.VirtualAddr + (PInst^.NextInst - PInst^.Addr)
  else
    VA := PInst^.NextInst;
  if (PInst^.ModRm.Value.&Mod = $00) and (PInst^.ModRm.Value.Rm = $05) then
  begin
    { Only displacement in memory
      ==> No registers . }
    if PInst^.Arch = CPUX64 then
    begin
      { EIP or RIP }
      F := F or JF_REL;
      case PInst^.AddressMode of
        AM_32:
          begin
            { Displacement = EIP + Offset }
            VA := PByte(UInt64(VA) and $FFFFFFFF);
          end;
        AM_64:
          begin
            { Displacement = RIP + Offset }
          end;
      else PInst^.Error(ERROR_INVALID_ADDRESS_MODE);

      end;
      P := VA + Int32(PInst^.Disp.Value);
      if DS = SEG_DS then
      begin
        try
          { Since UnivDisasm can disassemble from anywere (memory,disk,..)
            User may provide wrong address !!
            ==> So we must use try to access memory !!!
          }
          P := PByte(PUInt64(P)^);
        except
          P := nil;
          PInst^.Error(ERROR_INVALID_EFFECTIVE_ADDRESS);
        end;
        PInst^.DstAddr.Addr := P;
      end;
    end else begin
      { No EIP/RIP }
      if DS = SEG_DS then
      begin
        P := PByte(PInst^.Disp.Value);
        try
          case PInst^.AddressMode of
            AM_16: P := PByte(PUInt16(P)^);
            AM_32: P := PByte(PUInt32(P)^);
          else PInst^.Error(ERROR_INVALID_ADDRESS_MODE);
          end;
        except
          P := nil;
          PInst^.Error(ERROR_INVALID_EFFECTIVE_ADDRESS);
        end;
        PInst^.DstAddr.Addr := P;
      end;
    end;
  end else begin
    { JMP/CALL [REG] }
    F := F or JF_REG;
    PInst^.DstAddr.Addr := nil;
  end;
  PInst^.DstAddr.Flags := F;
end;

procedure DecodeBranch_Ap_w_z(PInst: PInstruction);
var
  Size: Byte;
begin
  PInst^.DstAddr.Flags := PInst^.DstAddr.Flags or JF_USED or JF_FAR;
  PInst^.Arg1.Flags := AF_ADDRESS;
  PInst^.Arg1.Size := (SIZE_WORD + PInst^.InternalData.OpSizeZ);
  PInst^.Arg1.Imm.Size := SIZE_WORD;
  PInst^.Arg1.Imm.Value := PInt16(PInst^.NextInst)^;
  if not PInst^.Next(SIZE_WORD) then
    Exit;
  Size := PInst^.InternalData.OpSizeZ;
  PInst^.Arg1.ImmEx.Size := Size;
  case Size of
    SIZE_WORD: PInst^.Arg1.ImmEx.Value := PInt16(PInst^.NextInst)^;
    SIZE_DWORD: PInst^.Arg1.ImmEx.Value := PInt32(PInst^.NextInst)^;
  else PInst^.Error(ERROR_INVALID_OPERAND_SIZE);
  end;
  PInst^.Next(Size);
end;

procedure DecodeBranch_Mp_w_z(PInst: PInstruction);
begin
  DecodeModRm(PInst);
  PInst^.DstAddr.Flags := PInst^.DstAddr.Flags or JF_USED or JF_FAR;
  PInst^.Arg1.Flags := AF_MEM;
  PInst^.Arg1.Size := (SIZE_WORD + PInst^.InternalData.OpSizeZ);
  DecodeArgAsMem(PInst, PInst^.Arg1);
end;

procedure DecodeBranch_Iw(PInst: PInstruction);
begin
  PInst^.DstAddr.Flags := PInst^.DstAddr.Flags or JF_USED or JF_FAR;
  PInst^.Arg1.Flags := AF_IMM;
  PInst^.Arg1.Size := SIZE_WORD;
  DecodeArgAsImm(PInst, PInst^.Arg1, SIZE_WORD);
end;

{ Memory Routine }

procedure DecodeArgAsMem(PInst: PInstruction; var Arg: TArgument);
const
  AM2Size: array [0 .. 3] of Byte = (0, //
    SIZE_WORD, //
    SIZE_DWORD, //
    SIZE_QWORD //
    );
  ModRm16ToBaseReg: array [0 .. 7] of TReg = ( //
    REG_BX, { 00 }
    REG_BX, { 01 }
    REG_BP, { 02 }
    REG_BP, { 03 }
    REG_SI, { 04 }
    REG_DI, { 05 }
    REG_BP, { 06 }
    REG_BX { 07 }
    );
  ModRm16ToIndexReg: array [0 .. 7] of TReg = ( //
    REG_SI, { 00 }
    REG_DI, { 01 }
    REG_SI, { 02 }
    REG_DI, { 03 }
    REG_NIL, { 04 }
    REG_NIL, { 05 }
    REG_NIL, { 06 }
    REG_NIL { 07 }
    );
var
  F: Byte;
  Sib: TByteSib;
  RegType: Word;
  N: UInt8;
begin
  RegType := REGS_GP or (AM2Size[PInst^.AddressMode] shl $08);
  F := PInst^.ModRm.Flags;
  if F and MF_EXTRA_MASK = MF_DEF_SS then
    Arg.Mem.Seg := SegOrDef(PInst, SEG_SS)
  else
    Arg.Mem.Seg := SegOrDef(PInst, SEG_DS);
  Sib := PInst^.Sib.Value;
  if PInst^.Sib.Flags and SF_USED <> 0 then
  begin
    if not((PInst^.ModRm.Value.&Mod = $00) and (Sib.Base = $05)) then
    begin
      Arg.Mem.BaseReg := RegType or Sib.Base or PInst^.Fields.B;
    end;
    if (Sib.Index <> $04) or (PInst^.Sib.Flags and SF_VSIB <> 0) then
    begin
      if PInst^.InternalData.VSibReg > 0 then
        Arg.Mem.IndexReg := PInst^.InternalData.VSibReg
      else
        Arg.Mem.IndexReg := RegType or Sib.Index or PInst^.Fields.X;
      Arg.Mem.Scale := Sib.Scale;
    end;
  end else begin
    if F and MF_BASE <> 0 then
    begin
      Arg.Mem.BaseReg := RegType or PInst^.ModRm.Value.Rm or PInst^.Fields.B;
    end;
  end;
  if PInst^.AddressMode = AM_16 then
  begin
    if not((PInst^.ModRm.Value.&Mod = $00) and (PInst^.ModRm.Value.Rm = $06)) then
    begin
      Arg.Mem.BaseReg := ModRm16ToBaseReg[PInst^.ModRm.Value.Rm];
      Arg.Mem.IndexReg := ModRm16ToIndexReg[PInst^.ModRm.Value.Rm];
    end;
  end;
  if (PInst^.Encoding = ENC_EVEX) and (PInst^.Disp.Size = SIZE_BYTE) then
  begin
    { Disp8 * N }
    N := GetDispN(PInst, Arg.Size);
    if N > 0 then
    begin
      PInst^.Disp.N := N;
      PInst^.Disp.Flags := DF_DISP8N;
      PInst^.Disp.Value := PInst^.Disp.Value * N;
      PInst^.Disp.Size := GetDispSizeN(PInst^.Disp.Value);
    end;
  end;

end;

procedure DecodeArgAsMemX(PInst: PInstruction; var Arg: TArgument; Size: Byte);
const
  AM2Reg: array [0 .. 3] of TReg = ( //
    0, { AM ?? }
    REG_SI, { AM 16 }
    REG_ESI, { AM 32 }
    REG_RSI { AM 64 }
    );
begin
  { DS : [EDI] }
  Arg.Mem.Seg := SEG_DS;
  Arg.Mem.BaseReg := AM2Reg[PInst^.AddressMode];
end;

procedure DecodeArgAsMemY(PInst: PInstruction; var Arg: TArgument; Size: Byte);
const
  AM2Reg: array [0 .. 3] of TReg = ( //
    0, { AM ?? }
    REG_DI, { AM 16 }
    REG_EDI, { AM 32 }
    REG_RDI { AM 64 }
    );
begin
  { ES : [EDI] }
  Arg.Mem.Seg := SEG_ES;
  Arg.Mem.BaseReg := AM2Reg[PInst^.AddressMode];
end;

{ Offset Routine }
procedure DecodeArgAsOffset(PInst: PInstruction; var Arg: TArgument; Size: Byte);
begin
  DecodeArgAsImm(PInst, Arg, AMToSize[PInst^.AddressMode]);
  Arg.Mem.Seg := SegOrDef(PInst, SEG_DS);
end;

{ Immediate Routine }

procedure DecodeArgAsImm(PInst: PInstruction; var Arg: TArgument; Size: Byte);
begin
  case Size of
    SIZE_BYTE: Arg.Imm.Value := PInt8(PInst^.NextInst)^;
    SIZE_WORD: Arg.Imm.Value := PInt16(PInst^.NextInst)^;
    SIZE_DWORD: Arg.Imm.Value := PInt32(PInst^.NextInst)^;
    SIZE_QWORD: Arg.Imm.Value := PInt64(PInst^.NextInst)^;
  end;
  PInst^.Next(Size);
  Arg.Imm.Size := Size;
end;

procedure DecodeImmAsOpExt(PInst: PInstruction);
begin
  PInst^.OpImmExt := PInst^.NextInst^;
  PInst^.Next();
end;

{ Validate Routine }
procedure ValidateBnd(PInst: PInstruction);
var
  F: Word;
begin
  if (PInst^.Prefixes.F2Prf.Flags and PF_MANDATORY <> 0) then
    Exit;
  F := PInst^.Prefixes.F2Prf.Flags;
  if F > 0 then
  begin
    F := F or PF_BND;
    if (PInst^.DstAddr.Flags and JF_NEAR <> 0) or ((PInst^.DstAddr.Flags and JF_SHORT <> 0) and (PInst^.DstAddr.Flags and JF_JCC <> 0)) then
    begin
      { BND prefix can be used only with:
        -near && short jcc.
        -near jmp.
        -near call && ret. }
      F := F or PF_VALID;
    end else begin
      PInst^.Warn(WARN_BND_NO_INIT);
    end;
  end;
  PInst^.Prefixes.F2Prf.Flags := F;
end;

procedure ValidateLock(PInst: PInstruction);
var
  L, F0, F2, F3: Word;
begin
  L := 0;
  F0 := PInst^.Prefixes.LockPrf.Flags; // LOCK
  F2 := PInst^.Prefixes.F2Prf.Flags; // XAQUIRE
  F3 := PInst^.Prefixes.F3Prf.Flags; // XRELEASE;
  if PInst^.ModRm.Value.IsReg then
  begin
    if (F0 and PF_USED <> 0) or (F2 and PF_USED <> 0) or (F3 and PF_USED <> 0) then
    begin
      PInst^.Warn(WARN_SOURCE_OPERAND_NOT_MEM);
      Exit;
    end;
  end;
  if (F0 and PF_USED <> 0) and (F0 and PF_MANDATORY = 0) then
  begin
    F0 := F0 or PF_VALID;
    L := PF_VALID;
  end;
  if PInst^.InstID = INST_ID_XCHG then
    L := PF_VALID;
  if (F2 and PF_USED <> 0) and (F2 and PF_MANDATORY = 0) then
  begin
    F2 := F2 or PF_XAQUIRE or L;
    if (F0 and PF_VALID = 0) and (PInst^.InstID <> INST_ID_XCHG) then
      PInst^.Warn(WARN_XAQUIRE_NEED_LOCK);
  end;
  if (F3 and PF_USED <> 0) and (F3 and PF_MANDATORY = 0) then
  begin
    F3 := F3 or PF_XRELEASE or L;
    if (F0 and PF_VALID = 0) and (PInst^.InstID <> INST_ID_XCHG) then
      PInst^.Warn(WARN_XRELEASE_NEED_LOCK);
  end;

  PInst^.Prefixes.LockPrf.Flags := F0; // LOCK
  PInst^.Prefixes.F2Prf.Flags := F2; // XAQUIRE
  PInst^.Prefixes.F3Prf.Flags := F3; // XRELEASE;
end;

procedure ValidateXRelease(PInst: PInstruction);
var
  F0, F3: Word;
begin
  { Only MOV instructions may call this procedure }
  { The MOV instruction is not lockable,however when it's combined with
    XRELEASE prefix , the LOCK prefix may be valid.
  }
  F0 := PInst^.Prefixes.LockPrf.Flags;
  F3 := PInst^.Prefixes.F3Prf.Flags;
  if (F3 and PF_USED <> 0) and (F3 and PF_MANDATORY = 0) then
  begin
    F3 := F3 or PF_XRELEASE;
    if PInst^.ModRm.Value.IsMem then
    begin
      F3 := F3 or PF_VALID;
      if (F0 and PF_USED) <> 0 then
        F0 := F0 or PF_VALID;
    end
    else
      PInst^.Warn(WARN_SOURCE_OPERAND_NOT_MEM);
  end;
  PInst^.Prefixes.LockPrf.Flags := F0;
  PInst^.Prefixes.F3Prf.Flags := F3;
end;

procedure ValidateRep(PInst: PInstruction);
begin
  if (PInst^.Prefixes.F3Prf.Flags and PF_MANDATORY <> 0) then
    Exit;

  if PInst^.Prefixes.F3Prf.Flags > 0 then
    PInst^.Prefixes.F3Prf.Flags := PInst^.Prefixes.F3Prf.Flags or PF_VALID or PF_REP;
end;

procedure ValidateRepe(PInst: PInstruction);
begin
  if (PInst^.Prefixes.F3Prf.Flags and PF_MANDATORY <> 0) then
    Exit;

  if PInst^.Prefixes.F3Prf.Flags > 0 then
    PInst^.Prefixes.F3Prf.Flags := PInst^.Prefixes.F3Prf.Flags or PF_VALID or PF_REPE;
end;

procedure ValidateRepne(PInst: PInstruction);
begin
  if (PInst^.Prefixes.F2Prf.Flags and PF_MANDATORY <> 0) then
    Exit;

  if PInst^.Prefixes.F2Prf.Flags > 0 then
    PInst^.Prefixes.F2Prf.Flags := PInst^.Prefixes.F2Prf.Flags or PF_VALID or PF_REPNE;
end;

{ Prefix Routine }

procedure CheckOpSize(PInst: PInstruction; Size: Byte);
begin
  if PInst^.Prefixes.OpSizePrf.Flags and PF_MANDATORY = 0 then
    SuperfluousPrefix(PInst, PInst^.Prefixes.OpSizePrf);
end;

procedure CheckSeg(PInst: PInstruction);
begin
  if PInst^.ModRm.Flags > 0 then
  begin
    if PInst^.ModRm.Value.Reg > $05 then
      PInst^.Error(ERROR_INVALID_SEGMENT);
  end;
end;

procedure SuperfluousPrefix(PInst: PInstruction; var P: TPrefix);
begin
  P.Flags := P.Flags or PF_SUPERFLUOUS;
  PInst^.Warn(WARN_SUPERFLUOUS_PREFIX);
end;

procedure MakeMndPrefix66(PInst: PInstruction);
begin
  PInst^.Prefixes.OpSizePrf.Flags := PInst^.Prefixes.OpSizePrf.Flags or PF_MANDATORY;
end;

procedure MakeMndPrefixF0(PInst: PInstruction);
begin
  PInst^.Prefixes.LockPrf.Flags := PInst^.Prefixes.LockPrf.Flags or PF_MANDATORY;
end;

procedure MakeMndPrefixF2(PInst: PInstruction);
begin
  PInst^.Prefixes.F2Prf.Flags := PInst^.Prefixes.F2Prf.Flags or PF_MANDATORY;
end;

procedure MakeMndPrefixF3(PInst: PInstruction);
begin
  PInst^.Prefixes.F3Prf.Flags := PInst^.Prefixes.F3Prf.Flags or PF_MANDATORY;
end;

{ Misc }
function VL2Regs(VLSize: Integer): TReg;
begin
  Result := REGS_XMM;
  if VLSize = SIZE_YWORD then
    Exit(REGS_YMM);
  if VLSize = SIZE_ZWORD then
    Exit(REGS_ZMM);
end;

procedure Decode_INVALID(PInst: PInstruction);
begin
  { OpCodes escaped here are not valid ! }
end;

end.
