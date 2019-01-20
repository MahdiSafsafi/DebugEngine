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
// The Original Code is UnivDisasm.Syntax.Utils.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Syntax.Utils;

interface

{$I Config.inc}

uses UnivDisasm.Disasm;

procedure fMoveChar(PInst: PInstruction; const c: UnivChar); {$IFDEF MustInline} inline; {$ENDIF}
procedure fMoveChars(PInst: PInstruction; const S: UnivString);
procedure MoveMnem(PInst: PInstruction; const Mnem: UnivString);
function GetRegName(ASyntax: Integer; Reg: TReg): UnivString;

implementation

uses
  UnivDisasm.SyntaxManager,
  UnivDisasm.Cnsts,
  UnivDisasm.Cnsts.Regs,
  UnivDisasm.Utils;

procedure fMoveChars(PInst: PInstruction; const S: UnivString);
var
  L: Integer;
begin
  L := Length(S);
  fMove(PByte(@PUnivChar(S)[0])^, Pointer(PInst^.InstStr)^, L);
  Inc(PInst^.InstStr, L);
end;

procedure fMoveChar(PInst: PInstruction; const c: UnivChar);
begin
  if c = #00 then
    Exit;
  PInst^.InstStr^ := c;
  Inc(PInst^.InstStr);
end;

procedure MoveMnem(PInst: PInstruction; const Mnem: UnivString);
var
  L: Integer;
  P: PByte;
begin
  L := Length(Mnem);
  P := PByte(PInst^.Mnem);
  fMove(PByte(@PUnivChar(Mnem)[0])^, Pointer(PInst^.Mnem)^, L);
  Inc(P, L);
  P^ := $00;
end;

function GetRegName(ASyntax: Integer; Reg: TReg): UnivString;
var
  id: Word;
  RegName: Pointer;
  PArray: PStrArray;
  P: PByte;
  PData: PSyntaxData;
begin
  PData := SyntaxManager.FData[ASyntax];
  if Reg and MagicRexMask <> 0 then
  begin
    P := PData^.UserRegsData^.LegacyRegs8R;
  end
  else if Reg and REGS_TYPE_MASK = REGS_GP then
  begin
    id := (Reg shr $8) and $F;
    P := PData^.LegacyRegs[id];
  end
  else
  begin
    id := Reg shr $C;
    P := PData^.RegsName[id];
  end;
  id := Reg and $1F;
  PArray := PStrArray(P);
  RegName := Pointer(PArray[id]);
  Result := UnivString(RegName);
end;

end.
