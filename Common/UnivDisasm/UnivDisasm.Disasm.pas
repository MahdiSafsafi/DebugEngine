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
// The Original Code is UnivDisasm.Disasm.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Disasm;

interface

{$I Config.inc}

uses
  SysUtils;

type
  UnivString = AnsiString;
  UnivChar = AnsiChar;
  PUnivChar = PAnsiChar;
  PInt8 = ^Int8;
  PInt16 = ^Int16;
  PInt32 = ^Int32;
  PInt64 = ^Int64;

  PUInt8 = ^UInt8;
  PUInt16 = ^UInt16;
  PUInt32 = ^UInt32;
  PUInt64 = ^UInt64;

  TStrArray = array [0 .. $F] of UnivString;
  PStrArray = ^TStrArray;

  TByteModRm = type Byte;
  TByteSib = type Byte;
  TVectorLength = type Byte;

  TReg = UInt16;

  TSib = record
    Flags: Byte; // Combination of SF_XX.
    Value: TByteSib; // Sib value.
  end;

  PSib = ^TSib;

  TModRm = record
    Flags: Byte; // Combination of MF_XX.
    Value: TByteModRm;
  end;

  PModRm = ^TModRm;

  TPrefix = record
    Flags: UInt16; // Combination of PF_XX.
    Count: UInt8; // Prefix repetition.
  end;

  PPrefix = ^TPrefix;

  TPrefixes = record
    Count: UInt8; // Totale prefixes count.
    OpSizePrf: TPrefix; // Operand size override prefix.
    AddrSizePrf: TPrefix; // Address size override prefix.
    SegFSPrf: TPrefix; // Segment FS override prefix.
    SegSSPrf: TPrefix; // Segment SS override prefix.
    SegGSPrf: TPrefix; // Segment GS override prefix.
    SegESPrf: TPrefix; // Segment ES override prefix.
    SegCSPrf: TPrefix; // Segment CS override prefix.
    SegDSPrf: TPrefix; // Segment DS override prefix.
    LockPrf: TPrefix; // Lock prefix.
    F2Prf: TPrefix; // REPNE,XAQUIRE,BND prefix #1.
    F3Prf: TPrefix; // REP,REPE,XRELEASE prefix #2.
    Rex: TPrefix; // REX prefix.
    DRex: TPrefix; // DREX prefix (AMD).
    VEXPrf: TPrefix; // VEX2,VEX3 prefix #3.
    EVEXPrf: TPrefix; // EVEX prefix.
    XOPPrf: TPrefix; // XOP prefix (AMD).
  end;

  {
    #1:
    F2Prf is REPNE when it's flags has PF_REPNE.
    F2Prf is XAQUIRE when it's flags has PF_XAQUIRE.
    F2Prf is BND when it's flags has PF_BND.

    #2:
    F3Prf is REPE when it's flags has PF_REPE.
    F3Prf is REP when it's flags has PF_REP.
    F3Prf is XRELEASE when it's flags has PF_XRELEASE.

    #3:
    VEXPrf has 3byte if it's flags marked with PF_VEX3.
    VEXPrf has 2byte if it's flags marked with PF_VEX2.
  }
  PPrefixes = ^TPrefixes;

  TDisplacement = record
    Flags: Byte;
    Size: Byte; // Displacement size #1
    Value: Int32; // Displacement value #1
    N: UInt8;
  end;

  {
    #1: if Disp.N > 0 then
    -The real displacement size encoded in memory is one byte.
    -The real displacement value encoded in memory is (Value / N).
  }
  PDisplacement = ^TDisplacement;

  TMemory = record
    Flags: UInt8;
    BaseReg: TReg;
    IndexReg: TReg;
    Scale: UInt8;
    Seg: TReg;
  end;

  PMemory = ^TMemory;

  TImmediate = record
    Value: Int64;
    Size: Byte; // Immediate size.
  end;

  PImmediate = ^TImmediate;

  TArgument = record
    Flags: Word; // Combination of AF_XX.
    Size: UInt8; // Argument size.
    Reg: TReg; // Argument register
    Imm: TImmediate;
    ImmEx: TImmediate;
    Mem: TMemory; // Memory.
  end;

  PArgument = ^TArgument;

  TInternalData = record
    OpSizeV: Byte;
    OpSizeY: Byte;
    OpSizeZ: Byte;
    VSibReg: TReg;
    DRex: Boolean;
    MandatoryPrefixes: Byte;
    MagicRex: Byte;
    Seg: TReg;
    SyntaxID: UInt8;
    Sp: Integer;
    Tuple: UInt8;
  end;

  PInternalData = ^TInternalData;

  TCommonPrfFields = record
    B: Byte; // (REX|VEX..).B
    X: Byte; // (REX|VEX..).X
    R: Byte; // (REX|VEX..).R
    W: Boolean; // (REX|VEX..).W
    Rp: Byte; // EVEX.R`
    Vp: Byte; // EVEX.V`
    z: Boolean; // EVEX.Zeroing.
    bc: Boolean; // EVEX.b.
    ocz0: Boolean; // DREX.oc0.
    vvvv: Byte;
    mmmmm: Byte;
    aaa: Byte; // Mask
    PP: Byte;
    Rnd: Byte;
    Dest: Byte; // DREX.Dest.
    VL: TVectorLength;
    function RRpExt: Byte;
    function XBExt: Byte;
  end;

  PCommonPrfFields = ^TCommonPrfFields;

  TDstAddress = record
    Flags: Word; // Combination of JF_XX.
    Addr: PByte; // Destination calculated address.
  end;

  PDstAddress = ^TDstAddress;

  TEFlags = packed record
    FOF: Byte; { EFLAGS.OF }
    FSF: Byte; { EFLAGS.SF }
    FZF: Byte; { EFLAGS.ZF }
    FAF: Byte; { EFLAGS.AF }
    FPF: Byte; { EFLAGS.PF }
    FCF: Byte; { EFLAGS.CF }
    FTF: Byte; { EFLAGS.TF }
    FIF: Byte; { EFLAGS.IF }
    FDF: Byte; { EFLAGS.DF }
    FNT: Byte; { EFLAGS.NT }
    FRF: Byte; { EFLAGS.RF }
    Reserved: Byte; // Reserved.
  end;

  PEFlags = ^TEFlags;

  TInstruction = record
    Arch: Byte; // CPUX32 || CPUX64 .
    Vendor: Byte; // One of VENDOR_XXX.
    Addr: PByte; // Address to start disassembling from.
    VirtualAddr: PByte; // Virtual address to calculate branch.
    NextInst: PByte; // Pointer to the next instruction.
    SafeLength: UInt8; // #5
    AddressMode: Byte; // One of AM_XX.
    Flags: UInt32; // Sets of GF_XXX.
    SyntaxOptions: Int32;
    Syntax: Int8;
    InstGroups: Word; // Sets of INST_GRP_XX.
    InstCategory: UInt32; // Sets of INST_CATEGORY_XX.
    FlagsIndex: UInt8; // EFlags index => Use Eflags property !
    InstID: Word; // One of INST_ID_XX.
    Encoding: Byte; // One of ENC_
    ModRm: TModRm;
    Disp: TDisplacement;
    Sib: TSib;
    Prefixes: TPrefixes;
    nArg: UInt8; // Arguments count.
    Arg1: TArgument;
    Arg2: TArgument;
    Arg3: TArgument;
    Arg4: TArgument;
    OpImmExt: Byte; // Extra immediate that serves as opcode extension.
    DstAddr: TDstAddress;
    Fields: TCommonPrfFields; // Common fields shared between prefixes.
    Mnem: PUnivChar; // MNEM_XXX.
    InstStr: PUnivChar; // Full instruction representation.
    InternalData: TInternalData; // Internal data used by UnivDisasm.==> Ignore.
    Warnings: UInt32; // Sets of WARN_XX.
    Errors: UInt32; // Sets of ERROR_XX.
    Options: Integer; // Sets of GO_
    UserTag: UInt64;
  private
    procedure SetSizeToF64();
    procedure SetSizeToD64();
    procedure SetSizeToDf64();
    function T2N2OpImmMask: Byte;
    function LowerVL: Byte;
    function UpperVL: Byte;
    function HalfVL: Byte;
    function FourthVL: Byte;
    function EighthVL: Byte;
    function GetEflags: TEFlags;
    function SupportFlag(AFlag: Word): Word;
    function IsOpcVPrefix: Boolean;
    procedure SetSp(Sp: Integer);
  public
    class function Create: TInstruction; static;
    procedure Free;
    function Next(const Size: UInt8 = 1): Boolean;
    function IsIA64: Boolean;
    procedure SetTable(Table: Byte);
    procedure SetGroup(Group: Word);
    procedure SetTupleType(ATuple: UInt8);
    procedure SetTuple4VL(ATuple: UInt8);
    procedure SetTuple4VL128(ATuple: UInt8);
    procedure SetTuple4VL256(ATuple: UInt8);
    procedure SetTuple4VL512(ATuple: UInt8);
    function SizeOfReg(Reg: TReg): Byte;
    procedure Warn(Code: UInt32);
    procedure Error(Code: UInt32);
    property Eflags: TEFlags read GetEflags;
  end;

  { #5: SafeLength is the number of byte that UnivDisasm
    can read since Addr.
  }
  PInstruction = ^TInstruction;

  TByteModRmHlp = record helper for TByteModRm
  private
    function GetMod: Byte;
    function GetReg: Byte;
    function GetRm: Byte;
  public
    property &Mod: Byte read GetMod;
    property Reg: Byte read GetReg;
    property Rm: Byte read GetRm;
    function IsMem: Boolean;
    function IsReg: Boolean;
  end;

  TByteSibHlp = record helper for TByteSib
  private
    function GetBase: Byte;
    function GetIndex: Byte;
    function GetScale: Byte;
  public
    property Base: Byte read GetBase;
    property Index: Byte read GetIndex;
    property Scale: Byte read GetScale;
  end;

  TVectorLengthHlp = record helper for TVectorLength
  public
    function Upper(): UInt8;
    function Lower(): UInt8;
    function Half(): UInt8;
    function Fourth(): UInt8;
    function Eighth(): UInt8;
  end;

  TDecoderProc = procedure(PInst: PInstruction);

function Disasm(PInst: PInstruction): ShortInt;

{$I Includes\OpCodes.dec.inc}
{$I Includes\OpCodes.tables.inc}

implementation

uses
  UnivDisasm.Cnsts.Mnemonics,
  UnivDisasm.Internal.Common,
  UnivDisasm.Internal.Prefixes,
  UnivDisasm.Internal.Escape,
  UnivDisasm.Cnsts,
  UnivDisasm.Cnsts.Instructions,
  UnivDisasm.Cnsts.Regs,
  UnivDisasm.SyntaxManager,
  UnivDisasm.Syntax.Utils;

procedure Decode_SSE_PREFETCH_M(PInst: PInstruction); forward;
procedure Decode_SSE_PREFETCHW_M(PInst: PInstruction); forward;
procedure Decode_SSE_PREFETCHWT1_Mb(PInst: PInstruction); forward;

const
  Size98or108Sel: array [0 .. 1] of Byte = (SIZE_98_BYTE, SIZE_108_BYTE);
  Size14or28Sel: array [0 .. 1] of Byte = (SIZE_14_BYTE, SIZE_28_BYTE);

  OpCodes_0x0F_0x0D_PREFETCH_Array: array [0 .. 7] of TDecoderProc = ( //
    Decode_SSE_PREFETCH_M, { 0 }
    Decode_SSE_PREFETCHW_M, { 1 }
    Decode_SSE_PREFETCHWT1_Mb, { 2 }
    Decode_SSE_PREFETCH_M, { 3 }
    Decode_SSE_PREFETCH_M, { 4 }
    Decode_SSE_PREFETCH_M, { 5 }
    Decode_SSE_PREFETCH_M, { 6 }
    Decode_SSE_PREFETCH_M { 7 }
    );
{$I EFLAGS_TABLE.inc}
{$I Includes\CommonDecoders.inc}
{$I Includes\GROUPS.dec.inc}
{$I Includes\AVX512DQ.inc}
{$I Includes\SSSE3.inc}
{$I Includes\AVX512PF-VSIB.inc}
{$I Includes\3DNOW.inc}
{$I Includes\BMI2.inc}
{$I Includes\SSE4V1.inc}
{$I Includes\AVX512IFMA-VL.inc}
{$I Includes\AVX512BW-VL.inc}
{$I Includes\F16C.inc}
{$I Includes\AVX512CD-VL.inc}
{$I Includes\SSE3.inc}
{$I Includes\MEM-SSE.inc}
{$I Includes\SSE.inc}
{$I Includes\AVX512BW.inc}
{$I Includes\SSE2.inc}
{$I Includes\SSE4V2.inc}
{$I Includes\ADX.inc}
{$I Includes\ICEBP.inc}
{$I Includes\MEM-SSE2.inc}
{$I Includes\FMA4.inc}
{$I Includes\SYSTEM.inc}
{$I Includes\MMX-SSE2.inc}
{$I Includes\SHA.inc}
{$I Includes\AVX512F-VL-VSIB.inc}
{$I Includes\MMX-SSE.inc}
{$I Includes\TBM.inc}
{$I Includes\AVX512DQ-VL.inc}
{$I Includes\SSE4A.inc}
{$I Includes\AVX512ER.inc}
{$I Includes\MMX-SSSE3.inc}
{$I Includes\SMM.inc}
{$I Includes\SSE5A.inc}
{$I Includes\VMX.inc}
{$I Includes\AVX.inc}
{$I Includes\XOP.inc}
{$I Includes\AVX2.inc}
{$I Includes\GP.inc}
{$I Includes\AVX512F.inc}
{$I Includes\BMI.inc}
{$I Includes\MPX.inc}
{$I Includes\AVX512F-VL.inc}
{$I Includes\LWP.inc}
{$I Includes\INVPCID.inc}
{$I Includes\FPU.inc}
{$I Includes\FMA.inc}
{$I Includes\MMX.inc}
{$I Includes\TSX.inc}
{$I Includes\AVX2-VSIB.inc}
{$I Includes\PCLMUL.inc}
{$I Includes\INSX.inc}
{$I Includes\SSE4V1-SSE5A.inc}
{$I Includes\VME.inc}
{$I Includes\AVX512VBMI-VL.inc}
{$I Includes\AES.inc}
{$I Includes\OpCodes.inc}
{$I Includes\GROUPS.inc}

const
  DefAddressMode: array [0 .. 1] of Byte = (AM_32, AM_64);

procedure AnalyseAF(PInst: PInstruction);
var
  F: Word;
begin
  F := PInst^.Prefixes.LockPrf.Flags;
  if (F and PF_USED <> 0) and (F and (PF_MANDATORY or PF_VALID) = 0) then
    PInst^.Warn(WARN_INST_NOT_LOCKABLE);
  F := PInst^.Prefixes.F2Prf.Flags;
  if (F and PF_USED <> 0) and (F and (PF_MANDATORY or PF_VALID) = 0) then
  begin
    case F and PF_TYPE_MASK of
      PF_BND:
        PInst^.Warn(WARN_BND_NO_INIT);
      PF_XAQUIRE:
        begin
          if PInst^.Warnings and WARN_XAQUIRE_NEED_LOCK = 0 then
            PInst^.Warn(WARN_XAQUIRE_INVALID);
        end;
      PF_REPNE:
        PInst^.Warn(WARN_REPNE_INVALID);
    end;
  end;
  F := PInst^.Prefixes.F3Prf.Flags;
  if (F and PF_USED <> 0) and (F and (PF_MANDATORY or PF_VALID) = 0) then
  begin
    case F and PF_TYPE_MASK of
      PF_XRELEASE:
        begin
          if PInst^.Warnings and WARN_XRELEASE_NEED_LOCK = 0 then
            PInst^.Warn(WARN_XRELEASE_INVALID);
        end;
      PF_REP:
        PInst^.Warn(WARN_REP_INVALID);
      PF_REPE:
        PInst^.Warn(WARN_REPE_INVALID);
    end;
  end;
end;

function Disasm(PInst: PInstruction): ShortInt;
(* const
  CPU2OpSizeY: array [0 .. 1] of Byte = (SIZE_DWORD, SIZE_QWORD);
*)
var
  P: PByte;
begin
  // Result := 0;
  PInst^.AddressMode := DefAddressMode[PInst^.Arch];
  PInst^.NextInst := PInst^.Addr;
  PInst^.InternalData.OpSizeY := SIZE_DWORD;
  PInst^.InternalData.OpSizeV := SIZE_DWORD;
  PInst^.InternalData.OpSizeZ := SIZE_DWORD;
  PInst^.Mnem := AllocMem(MAX_MNEM_LENGTH);
  GetMem(PInst^.InstStr, MAX_INST_STR_LENGTH);
  P := PByte(PInst^.InstStr);
  PInst^.SetTable(GF_TABLE_1);
  if PInst^.SafeLength = 0 then
    PInst^.SafeLength := $FF;
  PInst^.InternalData.SyntaxID := SyntaxManager.GetSyntaxIndex(PInst^.Syntax);
  PInst^.InstID := INST_ID_INVALID;
  PInst^.InternalData.MandatoryPrefixes := MND_PRF_NA;
  TABLE_1[PInst^.Addr^](PInst);
  Result := PInst^.NextInst - PInst^.Addr;
  AnalyseAF(PInst);
  if PInst^.InstID = INST_ID_INVALID then
  begin
    PInst^.Error(ERROR_INVALID_OPCODE);
{$IFDEF NEED_DISPLAY}
    MoveMnem(PInst, MNEM_INVALID);
    SyntaxManager.SyntaxDecoderArray[PInst.InternalData.SyntaxID](PInst);
{$ENDIF NEED_DISPLAY}
  end;
  PInst^.InstStr := PAnsiChar(P);
end;

{ TInstruction }

class function TInstruction.Create: TInstruction;
begin
  Result := default (TInstruction);
end;

procedure TInstruction.Free;
begin
  if Assigned(Self.Mnem) then
    FreeMem(Self.Mnem);
  if Assigned(Self.InstStr) then
    FreeMem(Self.InstStr);
end;

function TInstruction.IsIA64: Boolean;
begin
  Result := (Vendor = VENDOR_INTEL) and (Arch = CPUX64);
end;

function TInstruction.IsOpcVPrefix: Boolean;
begin
  { Choose between (BOUND|LDS|LES) instructions or (EVEX|VEX2|VEX3) prefixes ! }
  if Arch = CPUX64 then
    Exit(True);
  Result := NextInst^ and $C0 = $C0;
end;

function TInstruction.Next(const Size: UInt8 = 1): Boolean;
begin
  Result := True;
  Inc(NextInst, Size);
  if (NextInst - Addr) > SafeLength then
  begin
    Dec(NextInst, Size);
    Self.Error(ERROR_EXCEEDED_SAFE_LENGTH);
    Result := False;
  end;
end;

procedure TInstruction.SetGroup(Group: Word);
begin
  Flags := Flags or Group;
end;

procedure TInstruction.SetSizeToD64;
begin
  if (Arch = CPUX64) then
  begin
    if (Prefixes.OpSizePrf.Flags and PF_USED <> 0) then
      InternalData.OpSizeV := SIZE_WORD
    else
      InternalData.OpSizeV := SIZE_QWORD;
  end;
end;

procedure TInstruction.SetSizeToDf64;
begin
  if Arch = CPUX64 then
  begin
    InternalData.OpSizeV := SIZE_QWORD;
    InternalData.OpSizeY := SIZE_QWORD;
  end;
  if (Vendor = VENDOR_AMD) and (Prefixes.OpSizePrf.Flags and PF_USED <> 0) then
    InternalData.OpSizeV := SIZE_WORD;
end;

procedure TInstruction.SetSizeToF64;
begin
  { Force Size to 64bit on PM64. }
  if Arch = CPUX64 then
  begin
    InternalData.OpSizeY := SIZE_QWORD;
    InternalData.OpSizeV := SIZE_QWORD;
  end;
end;

procedure TInstruction.SetSp(Sp: Integer);
begin
  InternalData.Sp := Sp;
end;

procedure TInstruction.SetTable(Table: Byte);
begin
  Flags := (Flags xor GF_TABLE_MASK) or Table;
end;

procedure TInstruction.SetTuple4VL(ATuple: UInt8);
begin
  if Fields.VL <> $00 then
    InternalData.Tuple := ATuple;
end;

procedure TInstruction.SetTuple4VL128(ATuple: UInt8);
begin
  if (InternalData.Tuple > 0) then
  begin
    if Fields.VL = VL128 then
      InternalData.Tuple := ATuple;
  end
  else
    InternalData.Tuple := ATuple;
end;

procedure TInstruction.SetTuple4VL256(ATuple: UInt8);
begin
  if (InternalData.Tuple > 0) then
  begin
    if Fields.VL = VL256 then
      InternalData.Tuple := ATuple;
  end
  else
    InternalData.Tuple := ATuple;
end;

procedure TInstruction.SetTuple4VL512(ATuple: UInt8);
begin
  if (InternalData.Tuple > 0) then
  begin
    if Fields.VL = VL512 then
      InternalData.Tuple := ATuple;
  end
  else
    InternalData.Tuple := ATuple;
end;

procedure TInstruction.SetTupleType(ATuple: UInt8);
begin
  InternalData.Tuple := ATuple;
end;

function TInstruction.SizeOfReg(Reg: TReg): Byte;
var
  RegType: Word;
  RegSize: Byte;
  CPUSize: Byte;
begin
  RegType := Reg and REGS_TYPE_MASK;
  RegSize := ((Reg shr $8) and $F);
  if Arch = CPUX64 then
    CPUSize := SIZE_QWORD
  else
    CPUSize := SIZE_DWORD;
  case RegType of
    REGS_GP:
      Exit(RegSize);
    REGS_SEG:
      Exit(SIZE_WORD);
    REGS_FPU:
      Exit(SIZE_QWORD);
    REGS_MMX:
      Exit(SIZE_QWORD);
    REGS_XMM:
      Exit(SIZE_OWORD);
    REGS_YMM:
      Exit(SIZE_YWORD);
    REGS_ZMM:
      Exit(SIZE_ZWORD);
    REGS_MASK:
      Exit(SIZE_QWORD);
    REGS_BND:
      Exit(SIZE_OWORD);
    REGS_DBG, REGS_CNTRL:
      Exit(CPUSize);
    REGS_TAB:
      Exit(SIZE_WORD);
  end;
  Result := 00;
end;

function TInstruction.SupportFlag(AFlag: Word): Word;
begin
  { Check if new intel syntax options fits instruction. }
  Result := $00;
  if Encoding <> ENC_EVEX then
    Exit;
  case AFlag of
    AF_MASK:
      begin
        if Fields.aaa > 0 then
          Exit(AFlag);
        Exit;
      end;
    AF_ZEROING:
      begin
        if Fields.z then
          Exit(AFlag);
        Exit;
      end;
  end;
  if Fields.bc then
  begin

    case AFlag of
      AF_SAE, AF_ROUNDING:
        begin
          if (ModRm.Flags > 0) and (ModRm.Value.IsReg) then
            Result := AFlag;
        end;
      AF_BROADCAST32, AF_BROADCAST64:
        begin
          if (ModRm.Flags > 0) and (ModRm.Value.IsMem) then
            Result := AFlag;
        end;
    end;
  end;
end;

function TInstruction.T2N2OpImmMask: Byte;
begin
  { Selector for MNEMccXX }
  if Encoding and ENC_V <> 0 then
    Exit($1F)
  else
    Exit($07);
end;

function TInstruction.UpperVL: Byte;
begin
  Result := Fields.VL.Upper;
  if not(Result in [SIZE_YWORD, SIZE_ZWORD]) then
    Self.Error(ERROR_VL_EXPECTED_UPPER);
end;

function TInstruction.LowerVL: Byte;
begin
  Result := Fields.VL.Lower;
  if not(Result in [SIZE_OWORD, SIZE_YWORD]) then
    Self.Error(ERROR_VL_EXPECTED_LOWER);
end;

function TInstruction.HalfVL: Byte;
begin
  Result := Fields.VL.Half;
end;

function TInstruction.FourthVL: Byte;
begin
  Result := Fields.VL.Fourth;
end;

function TInstruction.GetEflags: TEFlags;
begin
  { Return a clone of TEFlags struct .
    So user will never corrupt EflagsTable. }
  Result := EFlagsTable[Self.FlagsIndex];
end;

function TInstruction.EighthVL: Byte;
begin
  Result := Fields.VL.Eighth;
end;

procedure TInstruction.Error(Code: UInt32);
begin
  Errors := Errors or Code;
end;

procedure TInstruction.Warn(Code: UInt32);
begin
  Warnings := Warnings or Code;
end;

{ TVectorLengthHlp }

function TVectorLengthHlp.Eighth: UInt8;
const
  EighthArray: array [0 .. 4] of Byte = ( //
    $00, { }
    SIZE_WORD, { 128 }
    SIZE_DWORD, { 256 }
    $00, { }
    SIZE_QWORD { 512 }
    );
begin
  { Return 1/8 Length }
  Result := EighthArray[Self shr $04];
{$IFDEF DEV}
  Assert(Result in [SIZE_WORD, SIZE_DWORD, SIZE_QWORD]);
{$ENDIF DEV}
end;

function TVectorLengthHlp.Fourth: UInt8;
const
  FourthArray: array [0 .. 4] of Byte = ( //
    $00, { }
    SIZE_DWORD, { 128 }
    SIZE_QWORD, { 256 }
    $00, { }
    SIZE_OWORD { 512 }
    );
begin
  { Return 1/4 Length }
  Result := FourthArray[Self shr $04];
{$IFDEF DEV}
  Assert(Result in [SIZE_DWORD, SIZE_QWORD, SIZE_OWORD]);
{$ENDIF DEV}
end;

function TVectorLengthHlp.Half: UInt8;
begin
  { Return 1/2 Length }
  Result := Self shr 1;
{$IFDEF DEV}
  Assert(Result in [SIZE_QWORD, SIZE_OWORD, SIZE_YWORD]);
{$ENDIF DEV}
end;

function TVectorLengthHlp.Upper: UInt8;
begin
  Result := Self;
end;

function TVectorLengthHlp.Lower: UInt8;
begin
  Result := Self;
end;

{ TByteModRmHlp }

function TByteModRmHlp.GetMod: Byte;
begin
  Result := (Self and $C0) shr 6;
end;

function TByteModRmHlp.GetReg: Byte;
begin
  Result := (Self and $38) shr 3;
end;

function TByteModRmHlp.GetRm: Byte;
begin
  Result := Self and 7;
end;

function TByteModRmHlp.IsMem: Boolean;
begin
  Result := &Mod < 3;
end;

function TByteModRmHlp.IsReg: Boolean;
begin
  Result := &Mod = 3;
end;

{ TCommonPrfFields }

function TCommonPrfFields.RRpExt: Byte;
begin
  Result := R or Rp;
end;

function TCommonPrfFields.XBExt: Byte;
begin
  Result := (X shl 1) or B;
end;

{ TByteSibHlp }

function TByteSibHlp.GetBase: Byte;
begin
  Result := Self and 7;
end;

function TByteSibHlp.GetIndex: Byte;
begin
  Result := (Self and $38) shr 3;
end;

function TByteSibHlp.GetScale: Byte;
begin
  Result := (1 shl (Self shr 6));
end;

procedure AssertAligned(Size: Integer);
var
  i: Integer;
begin
  for i in [2, 4, 8] do
  begin
    if Size mod i = 0 then
      Exit;
  end;
  Assert(False, 'Size not aligned.');
end;

initialization

AssertAligned(SizeOf(TModRm));
AssertAligned(SizeOf(TSib));
AssertAligned(SizeOf(TModRm));
AssertAligned(SizeOf(TPrefix));
AssertAligned(SizeOf(TPrefixes));
AssertAligned(SizeOf(TDisplacement));
AssertAligned(SizeOf(TMemory));
AssertAligned(SizeOf(TImmediate));
AssertAligned(SizeOf(TArgument));
AssertAligned(SizeOf(TInternalData));
AssertAligned(SizeOf(TCommonPrfFields));
AssertAligned(SizeOf(TDstAddress));
AssertAligned(SizeOf(TEFlags));
AssertAligned(SizeOf(TInstruction));

end.
