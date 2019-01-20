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
// The Original Code is UnivDisasm.Syntax.UnivSyntax.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Syntax.UnivSyntax;

interface

{$I Config.inc}

uses
  UnivDisasm.Disasm,
  UnivDisasm.Cnsts,
  UnivDisasm.Cnsts.Regs,
  UnivDisasm.Utils,
  UnivDisasm.SyntaxManager,
  UnivDisasm.Syntax.Utils;

const
  { UnivSyntax }
  SX_UNIV_SYNTAX = $00;

  { UnivSyntax Options }
  USO_ZERO_PADDING = $02;
  USO_SHOW_PTR = $04;
  USO_SHOW_MEM_SIZE = $08;
  USO_SHOW_RELATIVE_DISP = $10;
  USO_SHOW_1_DISP = $20;
  USO_SHOW_DISP8 = $40;
  USO_SHOW_DST_ADDR = $80; // Show branch calculated address.
  USO_OFFSET_AS_MEM = $100; // Offset is treated as memory location.
  USO_DEFAULT = USO_SHOW_MEM_SIZE or USO_SHOW_RELATIVE_DISP or USO_ZERO_PADDING or USO_SHOW_DISP8 or USO_SHOW_DST_ADDR;

procedure UnivSyntax(PInst: PInstruction);
function GetUnivSyntaxData: PRegistersData; // {$IFDEF MustInline} inline; {$ENDIF}

implementation

var
  UnivData: TRegistersData;

const

  SegRegsName: array [0 .. $7] of UnivString = ( //
    'cs', { 00 }
    'ds', { 01 }
    'ss', { 02 }
    'es', { 03 }
    'fs', { 04 }
    'gs', { 05 }
    '??', { 06 }
    '??' { 07 }
    );

  InvalidRegsName: array [0 .. $1F] of UnivString = ( //
    '??', { 00 }
    '??', { 01 }
    '??', { 02 }
    '??', { 03 }
    '??', { 04 }
    '??', { 05 }
    '??', { 06 }
    '??', { 07 }
    '??', { 08 }
    '??', { 09 }
    '??', { 10 }
    '??', { 11 }
    '??', { 12 }
    '??', { 13 }
    '??', { 14 }
    '??', { 15 }
    '??', { 16 }
    '??', { 17 }
    '??', { 18 }
    '??', { 19 }
    '??', { 20 }
    '??', { 21 }
    '??', { 22 }
    '??', { 23 }
    '??', { 24 }
    '??', { 25 }
    '??', { 26 }
    '??', { 27 }
    '??', { 28 }
    '??', { 29 }
    '??', { 30 }
    '??' { 31 } );

  Legacy8RegsName: array [0 .. $7] of UnivString = ( //
    'al', { 00 }
    'cl', { 01 }
    'dl', { 02 }
    'bl', { 03 }
    'ah', { 04 }
    'ch', { 05 }
    'dh', { 06 }
    'bh' { 07 }
    );

  Legacy8RexRegsName: array [0 .. $F] of UnivString = ( //
    'al', { 00 }
    'cl', { 01 }
    'dl', { 02 }
    'bl', { 03 }
    'spl', { 04 }
    'bpl', { 05 }
    'sil', { 06 }
    'dil', { 07 }
    'r8b', { 08 }
    'r9b', { 09 }
    'r10b', { 10 }
    'r11b', { 11 }
    'r12b', { 12 }
    'r13b', { 13 }
    'r14b', { 14 }
    'r15b' { 15 }
    );

  Legacy16RegsName: array [0 .. $F] of UnivString = ( //
    'ax', { 00 }
    'cx', { 01 }
    'dx', { 02 }
    'bx', { 03 }
    'sp', { 04 }
    'bp', { 05 }
    'si', { 06 }
    'di', { 07 }
    'r8w', { 08 }
    'r9w', { 09 }
    'r10w', { 10 }
    'r11w', { 11 }
    'r12w', { 12 }
    'r13w', { 13 }
    'r14w', { 14 }
    'r15w' { 15 }
    );

  Legacy32RegsName: array [0 .. $F] of UnivString = ( //
    'eax', { 00 }
    'ecx', { 01 }
    'edx', { 02 }
    'ebx', { 03 }
    'esp', { 04 }
    'ebp', { 05 }
    'esi', { 06 }
    'edi', { 07 }
    'r8d', { 08 }
    'r9d', { 09 }
    'r10d', { 10 }
    'r11d', { 11 }
    'r12d', { 12 }
    'r13d', { 13 }
    'r14d', { 14 }
    'r15d' { 15 }
    );

  Legacy64RegsName: array [0 .. $F] of UnivString = ( //
    'rax', { 00 }
    'rcx', { 01 }
    'rdx', { 02 }
    'rbx', { 03 }
    'rsp', { 04 }
    'rbp', { 05 }
    'rsi', { 06 }
    'rdi', { 07 }
    'r8', { 08 }
    'r9', { 09 }
    'r10', { 10 }
    'r11', { 11 }
    'r12', { 12 }
    'r13', { 13 }
    'r14', { 14 }
    'r15' { 15 }
    );

  FPURegsName: array [0 .. $07] of UnivString = ( //
    'st(0)', { 00 }
    'st(1)', { 01 }
    'st(2)', { 02 }
    'st(3)', { 03 }
    'st(4)', { 04 }
    'st(5)', { 05 }
    'st(6)', { 06 }
    'st(7)' { 07 } );

  MMXRegsName: array [0 .. $0F] of UnivString = ( //
    'mm0', { 00 }
    'mm1', { 01 }
    'mm2', { 02 }
    'mm3', { 03 }
    'mm4', { 04 }
    'mm5', { 05 }
    'mm6', { 06 }
    'mm7', { 07 }
    'mm8', { 08 }
    'mm9', { 09 }
    'mm10', { 10 }
    'mm11', { 11 }
    'mm12', { 12 }
    'mm13', { 13 }
    'mm14', { 14 }
    'mm15' { 15 } );

  CntrlRegsName: array [0 .. $0F] of UnivString = ( //
    'cr0', { 00 }
    'cr1', { 01 }
    'cr2', { 02 }
    'cr3', { 03 }
    'cr4', { 04 }
    'cr5', { 05 }
    'cr6', { 06 }
    'cr7', { 07 }
    'cr8', { 08 }
    'cr9', { 09 }
    'cr10', { 10 }
    'cr11', { 11 }
    'cr12', { 12 }
    'cr13', { 13 }
    'cr14', { 14 }
    'cr15' { 15 } );

  DbgRegsName: array [0 .. $0F] of UnivString = ( //
    'dr0', { 00 }
    'dr1', { 01 }
    'dr2', { 02 }
    'dr3', { 03 }
    'dr4', { 04 }
    'dr5', { 05 }
    'dr6', { 06 }
    'dr7', { 07 }
    'dr8', { 08 }
    'dr9', { 09 }
    'dr10', { 10 }
    'dr11', { 11 }
    'dr12', { 12 }
    'dr13', { 13 }
    'dr14', { 14 }
    'dr15' { 15 } );

  TableRegsName: array [0 .. $07] of UnivString = ( //
    'gdtr', { 00 }
    'idtr', { 01 }
    'ldtr', { 02 }
    'tr', { 03 }
    '??', { 04 }
    '??', { 05 }
    '??', { 06 }
    '??' { 07 }
    );

  XMMRegsName: array [0 .. $1F] of UnivString = ( //
    'xmm0', { 00 }
    'xmm1', { 01 }
    'xmm2', { 02 }
    'xmm3', { 03 }
    'xmm4', { 04 }
    'xmm5', { 05 }
    'xmm6', { 06 }
    'xmm7', { 07 }
    'xmm8', { 08 }
    'xmm9', { 09 }
    'xmm10', { 10 }
    'xmm11', { 11 }
    'xmm12', { 12 }
    'xmm13', { 13 }
    'xmm14', { 14 }
    'xmm15', { 15 }
    'xmm16', { 16 }
    'xmm17', { 17 }
    'xmm18', { 18 }
    'xmm19', { 19 }
    'xmm20', { 20 }
    'xmm21', { 21 }
    'xmm22', { 22 }
    'xmm23', { 23 }
    'xmm24', { 24 }
    'xmm25', { 25 }
    'xmm26', { 26 }
    'xmm27', { 27 }
    'xmm28', { 28 }
    'xmm29', { 29 }
    'xmm30', { 30 }
    'xmm31' { 31 } );

  YMMRegsName: array [0 .. $1F] of UnivString = ( //
    'ymm0', { 00 }
    'ymm1', { 01 }
    'ymm2', { 02 }
    'ymm3', { 03 }
    'ymm4', { 04 }
    'ymm5', { 05 }
    'ymm6', { 06 }
    'ymm7', { 07 }
    'ymm8', { 08 }
    'ymm9', { 09 }
    'ymm10', { 10 }
    'ymm11', { 11 }
    'ymm12', { 12 }
    'ymm13', { 13 }
    'ymm14', { 14 }
    'ymm15', { 15 }
    'ymm16', { 16 }
    'ymm17', { 17 }
    'ymm18', { 18 }
    'ymm19', { 19 }
    'ymm20', { 20 }
    'ymm21', { 21 }
    'ymm22', { 22 }
    'ymm23', { 23 }
    'ymm24', { 24 }
    'ymm25', { 25 }
    'ymm26', { 26 }
    'ymm27', { 27 }
    'ymm28', { 28 }
    'ymm29', { 29 }
    'ymm30', { 30 }
    'ymm31' { 31 } );

  ZMMRegsName: array [0 .. $1F] of UnivString = ( //
    'zmm0', { 00 }
    'zmm1', { 01 }
    'zmm2', { 02 }
    'zmm3', { 03 }
    'zmm4', { 04 }
    'zmm5', { 05 }
    'zmm6', { 06 }
    'zmm7', { 07 }
    'zmm8', { 08 }
    'zmm9', { 09 }
    'zmm10', { 10 }
    'zmm11', { 11 }
    'zmm12', { 12 }
    'zmm13', { 13 }
    'zmm14', { 14 }
    'zmm15', { 15 }
    'zmm16', { 16 }
    'zmm17', { 17 }
    'zmm18', { 18 }
    'zmm19', { 19 }
    'zmm20', { 20 }
    'zmm21', { 21 }
    'zmm22', { 22 }
    'zmm23', { 23 }
    'zmm24', { 24 }
    'zmm25', { 25 }
    'zmm26', { 26 }
    'zmm27', { 27 }
    'zmm28', { 28 }
    'zmm29', { 29 }
    'zmm30', { 30 }
    'zmm31' { 31 } );

  KRegsName: array [0 .. $07] of UnivString = ( //
    'k0', { 00 }
    'k1', { 01 }
    'k2', { 02 }
    'k3', { 03 }
    'k4', { 04 }
    'k5', { 05 }
    'k6', { 06 }
    'k7' { 07 } );

  MaskRegsName: array [0 .. $07] of UnivString = ( //
    '{k0}', { 00 }
    '{k1}', { 01 }
    '{k2}', { 02 }
    '{k3}', { 03 }
    '{k4}', { 04 }
    '{k5}', { 05 }
    '{k6}', { 06 }
    '{k7}' { 07 } );

  BoundRegsName: array [0 .. $07] of UnivString = ( //
    'bnd0', { 00 }
    'bnd1', { 01 }
    'bnd2', { 02 }
    'bnd3', { 03 }
    'bnd4', { 04 }
    'bnd5', { 05 }
    'bnd6', { 06 }
    'bnd7' { 07 } );

  ScaleToChar: array [0 .. 8] of UnivChar = ( //
    '0', { 00 }
    '1', { 01 }
    '2', { 02 }
    '3', { 03 }
    '4', { 04 }
    '5', { 05 }
    '6', { 06 }
    '7', { 07 }
    '8' { 08 }
    );

  LegacyRegsNameArrayPtr: array [0 .. 8] of PByte = ( //
    nil, { 00 }
    @Legacy8RegsName, { 01 }
    @Legacy16RegsName, { 02 }
    nil, { 03 }
    @Legacy32RegsName, { 04 }
    nil, { 05 }
    nil, { 06 }
    nil, { 07 }
    @Legacy64RegsName { 08 }
    );

  RegsNameArrayPtr: array [0 .. 14] of PByte = ( //
    @InvalidRegsName, { }
    @InvalidRegsName, { }
    @FPURegsName, { }
    @KRegsName, { }
    @MMXRegsName, { }
    @XMMRegsName, { }
    @YMMRegsName, { }
    @ZMMRegsName, { }
    @CntrlRegsName, { }
    @DbgRegsName, { }
    @TableRegsName, { }
    @BoundRegsName, { }
    @SegRegsName, { }
    @InvalidRegsName, { }
    @InvalidRegsName { }
    );

function Size2Str(ASize: Integer): UnivString;
begin
  Result := '';
  case ASize of
    SIZE_BYTE: Exit('byte');
    SIZE_WORD: Exit('word');
    SIZE_DWORD: Exit('dword');
    SIZE_QWORD: Exit('qword');
    SIZE_OWORD: Exit('oword');
    SIZE_YWORD: Exit('yword');
    SIZE_ZWORD: Exit('zword');
    SIZE_FWORD: Exit('fword');
    SIZE_TBYTE: Exit('tbyte');
  end;
end;

function _ufIntToHex(Value: UInt64; Digits: Integer; PInst: PInstruction): Integer;
var
  Offset: Integer;
begin
  Offset := 0;
  if Value > 9 then
  begin
    fMoveChars(PInst, '0x');
    Offset := 2;
  end;
  Result := fIntToHex(Value, Digits, PInst^.InstStr);
  Inc(PInst^.InstStr, Result);
  Inc(Result, Offset);
end;

function ufIntToHex(Value: UInt64; Digits: Integer; PInst: PInstruction): Integer; overload; {$IFDEF MustInline} inline; {$ENDIF}
begin
  Result := _ufIntToHex(Value, Digits, PInst);
end;

function ufIntToHex(Value: Int64; Digits: Integer; PInst: PInstruction): Integer; overload; {$IFDEF MustInline} inline; {$ENDIF}
begin
  Result := _ufIntToHex(Value, Digits, PInst);
end;

function ufIntToHex(Value: Integer; Digits: Integer; PInst: PInstruction): Integer; overload; {$IFDEF MustInline} inline; {$ENDIF}
begin
  Result := _ufIntToHex(Cardinal(Value), Digits, PInst);
end;

procedure ArgSyntax(PInst: PInstruction; var Arg: TArgument);
const
  AM2XIP: array [0 .. 3] of UnivString = ( //
    '??', { }
    'ip', { AM16 }
    'eip', { AM32 }
    'rip' { AM64 }
    );
  Rnd2Str: array [0 .. 4] of UnivString = ( //
    '', //
    ',{rn-sae}', //
    ',{rd-sae}', //
    ',{ru-sae}', //
    ',{rz-sae}' //
    );
  BroadcastDecorate: array [1 .. 2] of UnivString = ('{1to16}', '{1to8}');
  CPUX2Size: array [0 .. 1] of UInt8 = (4, 8);
  ZPSel: array [Boolean] of UInt8 = (0, 2);
  ScaleccSel: array [Boolean] of UInt8 = (1, 0);

var
  S: UnivString;
  Broadcast: Byte;
  L: Integer;
  Sign: UnivChar;
  DispValue: Int32;
  Options: UInt32;
  nZ, CPUSize, Scalecc: UInt8;
  OffsetAsMem: Boolean;
begin
  if Arg.Flags = 0 then
    Exit;

  Options := PInst^.SyntaxOptions;
  Broadcast := 0;
  Sign := #00;
  nZ := ZPSel[Options and USO_ZERO_PADDING <> 0];
  Scalecc := ScaleccSel[Options and USO_SHOW_1_DISP <> 0];
  CPUSize := CPUX2Size[PInst^.Arch];
  OffsetAsMem := (Options and USO_OFFSET_AS_MEM <> 0) and (Arg.Flags and AF_TYPE_MASK = AF_OFFSET);
  if Arg.Flags and AF_TYPE_MASK = AF_REG then
  begin
    S := GetRegName(PInst^.Syntax, Arg.Reg);
    fMoveChars(PInst, S);
  end
  else if (Options and USO_SHOW_DST_ADDR <> 0) and (Assigned(PInst^.DstAddr.Addr)) then
  begin
    if PInst^.DstAddr.Flags and JF_FAR <> 0 then
      fMoveChars(PInst, 'far ');
    fMoveChar(PInst, '@');
    ufIntToHex(UInt64(PInst^.DstAddr.Addr), CPUSize * nZ, PInst);
  end
  else if ((Arg.Flags and AF_TYPE_MASK = AF_MEM) or (OffsetAsMem)) then
  begin
    if Options and USO_SHOW_MEM_SIZE <> 0 then
    begin
      if (PInst^.Fields.bc) and (Arg.Flags and AF_BROADCAST32 = AF_BROADCAST32) then
      begin
        Broadcast := 1;
        fMoveChars(PInst, 'dword ');
      end
      else if (PInst^.Fields.bc) and (Arg.Flags and AF_BROADCAST64 = AF_BROADCAST64) then
      begin
        Broadcast := 2;
        fMoveChars(PInst, 'qword ');
      end
      else if ((Arg.Size > 0) and (Arg.Size <= SIZE_ZWORD)) then
      begin
        fMoveChars(PInst, Size2Str(Arg.Size));
        fMoveChar(PInst, ' ');
      end;
    end;
    if Options and USO_SHOW_PTR <> 0 then
      fMoveChars(PInst, 'ptr ');
    if Arg.Mem.Seg > 0 then
    begin
      // fMoveChar(PInst, ' ');
      fMoveChars(PInst, GetRegName(PInst^.Syntax, Arg.Mem.Seg));
      fMoveChar(PInst, ':');
    end;
    if (PInst^.Disp.N > 0) and (Options and USO_SHOW_DISP8 <> 0) then
      fMoveChars(PInst, 'disp8');
    fMoveChar(PInst, '[');
    if OffsetAsMem then
    begin
      ufIntToHex(Arg.Imm.Value, Arg.Size * nZ, PInst);
    end
    else
    begin
      if (PInst^.Disp.Flags and DF_REL <> 0) and (Options and USO_SHOW_RELATIVE_DISP <> 0) then
      begin
        fMoveChars(PInst, AM2XIP[PInst^.AddressMode]);
        fMoveChar(PInst, ' ');
      end;
      if Arg.Mem.BaseReg > 0 then
      begin
        S := GetRegName(PInst^.Syntax, Arg.Mem.BaseReg);
        fMoveChars(PInst, S);
        Sign := '+';
      end;
      if Arg.Mem.IndexReg > 0 then
      begin
        fMoveChar(PInst, Sign);
        S := GetRegName(PInst^.Syntax, Arg.Mem.IndexReg);
        fMoveChars(PInst, S);
        if Arg.Mem.Scale > Scalecc then
        begin
          fMoveChar(PInst, '*');
          fMoveChars(PInst, ScaleToChar[Arg.Mem.Scale]);
        end;
        Sign := '+';
      end;
      if PInst^.Disp.Size > 0 then
      begin
        if Sign <> #00 then
        begin
          if PInst^.Disp.Value < 0 then
          begin
            DispValue := -PInst^.Disp.Value;
            Sign := '-';
          end
          else
            DispValue := PInst^.Disp.Value;
          fMoveChar(PInst, Sign);
          fMoveChars(PInst, '0x');
          L := fIntToHex(DispValue, PInst^.Disp.Size * nZ, PInst^.InstStr);
          Inc(PInst^.InstStr, L);
        end
        else
        begin
          fMoveChars(PInst, '0x');
          L := fIntToHex(PInst^.Disp.Value, PInst^.Disp.Size * nZ, PInst^.InstStr);
          Inc(PInst^.InstStr, L);
        end;
      end;
    end;
    fMoveChar(PInst, ']');
    if Broadcast > 0 then
      fMoveChars(PInst, BroadcastDecorate[Broadcast]);
  end
  else if Arg.Flags and AF_TYPE_MASK = AF_IMM then
  begin
    ufIntToHex(Arg.Imm.Value, Arg.Size * nZ, PInst);
  end
  else if ((Arg.Flags and AF_TYPE_MASK = AF_OFFSET) and (not OffsetAsMem)) then
  begin
    if Arg.Mem.Seg > 0 then
    begin
      fMoveChar(PInst, ' ');
      fMoveChars(PInst, GetRegName(PInst^.Syntax, Arg.Mem.Seg));
      fMoveChar(PInst, ':');
    end;
    ufIntToHex(Arg.Imm.Value, Arg.Size * nZ, PInst);
  end
  else if Arg.Flags and AF_TYPE_MASK = AF_ADDRESS then
  begin
    ufIntToHex(Arg.Imm.Value, Arg.Imm.Size * nZ, PInst);
    fMoveChar(PInst, ':');
    ufIntToHex(Arg.ImmEx.Value, Arg.ImmEx.Size * nZ, PInst);
  end
  else if Arg.Flags and AF_TYPE_MASK = AF_RELATIVE_OFFSET then
  begin
    if PInst^.DstAddr.Flags and JF_SHORT <> 0 then
      fMoveChars(PInst, 'short ');
    if Assigned(PInst^.DstAddr.Addr) then
    begin
      fMoveChar(PInst, '@');
      L := fIntToHex(UInt64(PInst^.DstAddr.Addr), CPUSize * nZ, PInst^.InstStr);
      Inc(PInst^.InstStr, L);
    end
    else
    begin
      ufIntToHex(Arg.Imm.Value, Arg.Imm.Size * nZ, PInst);
    end;
    fMoveChars(PInst, S);
  end;

  if PInst^.Encoding = ENC_EVEX then
  begin
    if (Arg.Flags and AF_MASK <> 0) then
      fMoveChars(PInst, MaskRegsName[PInst^.Fields.aaa]);
    if (Arg.Flags and AF_ZEROING <> 0) then
      fMoveChars(PInst, '{z}');

    if (PInst^.Fields.bc) then
    begin
      if Arg.Flags and AF_SAE = AF_SAE then
        fMoveChars(PInst, ',{sae}')
      else if Arg.Flags and AF_ROUNDING = AF_ROUNDING then
        fMoveChars(PInst, Rnd2Str[PInst^.Fields.Rnd]);
    end;
  end;
  fMoveChar(PInst, ',');

end;

procedure UnivSyntax(PInst: PInstruction);
var
  PrfID: Byte;
begin
  try
    try
      if PInst^.Prefixes.F2Prf.Flags and PF_VALID <> 0 then
      begin
        PrfID := PInst^.Prefixes.F2Prf.Flags and PF_TYPE_MASK;
        case PrfID of
          PF_REPNE: fMoveChars(PInst, 'repne ');
          PF_XAQUIRE: fMoveChars(PInst, 'xaquire ');
          PF_BND: fMoveChars(PInst, 'bnd ');
        end;
      end;
      if PInst^.Prefixes.F3Prf.Flags and PF_VALID <> 0 then
      begin
        PrfID := PInst^.Prefixes.F3Prf.Flags and PF_TYPE_MASK;
        case PrfID of
          PF_REP: fMoveChars(PInst, 'rep ');
          PF_REPE: fMoveChars(PInst, 'repe ');
          PF_XRELEASE: fMoveChars(PInst, 'xrelease ');
        end;
      end;
      if PInst^.Prefixes.LockPrf.Flags and PF_VALID <> 0 then
      begin
        fMoveChars(PInst, 'lock ');
      end;
      fMoveChars(PInst, PInst^.Mnem);
      fMoveChar(PInst, ' ');
      ArgSyntax(PInst, PInst^.Arg1);
      ArgSyntax(PInst, PInst^.Arg2);
      ArgSyntax(PInst, PInst^.Arg3);
      ArgSyntax(PInst, PInst^.Arg4);
      Dec(PInst^.InstStr);
    finally
      PInst^.InstStr^ := #00;
    end;
  except
    PInst^.Error(ERROR_INTERNAL);
  end;
end;

function GetUnivSyntaxData: PRegistersData;
begin
  Result := @UnivData;
end;

initialization

UnivData.LegacyRegs8 := @Legacy8RegsName;
UnivData.LegacyRegs8R := @Legacy8RexRegsName;
UnivData.LegacyRegs16 := @Legacy16RegsName;
UnivData.LegacyRegs32 := @Legacy32RegsName;
UnivData.LegacyRegs64 := @Legacy64RegsName;
UnivData.FPURegs := @FPURegsName;
UnivData.CntrlRegs := @CntrlRegsName;
UnivData.DbgRegs := @DbgRegsName;
UnivData.TableRegs := @TableRegsName;
UnivData.MMXRegs := @MMXRegsName;
UnivData.XMMRegs := @XMMRegsName;
UnivData.YMMRegs := @YMMRegsName;
UnivData.ZMMRegs := @ZMMRegsName;
UnivData.KRegs := @KRegsName;
UnivData.MaskRegs := @MaskRegsName;
UnivData.BoundRegs := @BoundRegsName;
UnivData.SegRegs := @SegRegsName;
UnivData.InvalidRegs := @InvalidRegsName;

end.
