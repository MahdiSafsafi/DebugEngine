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
// The Original Code is UnivDisasm.SyntaxManager.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.SyntaxManager;

interface

{$I Config.inc}
{$HINTS Off}

uses
  Generics.Collections,
  UnivDisasm.Disasm;

const
  { Max allowed syntax }
  MAX_SYNTAX_DECODER = $10;

  { SyntaxManager ERRORS }
  SME_INDEX_NOT_FOUND = -1;
  SME_SYNTAX_ALREADY_EXISTS = -2;
  SME_REGISTER_SYNTAX_FAILED = -3;
  SME_EXCEEDED_MAX_SYNTAX_DECODER = -3;

type

  TRegistersData = record
    LegacyRegs8: PByte;
    LegacyRegs8R: PByte;
    LegacyRegs16: PByte;
    LegacyRegs32: PByte;
    LegacyRegs64: PByte;
    FPURegs: PByte;
    CntrlRegs: PByte;
    DbgRegs: PByte;
    MMXRegs: PByte;
    XMMRegs: PByte;
    YMMRegs: PByte;
    ZMMRegs: PByte;
    KRegs: PByte;
    MaskRegs: PByte;
    BoundRegs: PByte;
    TableRegs: PByte;
    SegRegs: PByte;
    InvalidRegs: PByte;
  end;

  PRegistersData = ^TRegistersData;

  TSyntaxData = record
    UserRegsData: PRegistersData;
    LegacyRegs: array [0 .. 8] of PByte;
    RegsName: array [0 .. 14] of PByte;
  end;

  PSyntaxData = ^TSyntaxData;

  TSyntaxManager = record
  private
    FLock: TObject;
    FNextSyntax: Integer;
    FRegisteredSyntax: TDictionary<Integer, Integer>;
    procedure Init;
    procedure Die;
  public
    FData: TDictionary<Integer, PSyntaxData>;
    SyntaxDecoderArray: array [0 .. MAX_SYNTAX_DECODER] of TDecoderProc;
    function GetSyntaxIndex(ASyntax: Integer): Integer;
    function RegisterSyntax(ASyntax: Integer; SyntaxProc: TDecoderProc; PRegsData: PRegistersData): Integer;
    procedure Enter;
    procedure Leave;
  end;

var
  SyntaxManager: TSyntaxManager = (FNextSyntax: - 1);

implementation

uses
  UnivDisasm.Syntax.UnivSyntax,
  UnivDisasm.Syntax.NilSyntax;

{ TSyntaxManager }

procedure TSyntaxManager.Die;
var
  Value: PSyntaxData;
begin
  FLock.Free;
  FRegisteredSyntax.Free;
  for Value in FData.Values do
  begin
    FreeMem(Value);
  end;
  FData.Free;
end;

procedure TSyntaxManager.Enter;
begin
  TMonitor.Enter(FLock);
end;

function TSyntaxManager.GetSyntaxIndex(ASyntax: Integer): Integer;
begin
  Result := SME_INDEX_NOT_FOUND;
  if FRegisteredSyntax.ContainsKey(ASyntax) then
    Result := FRegisteredSyntax[ASyntax];
end;

procedure TSyntaxManager.Init;
begin
  FLock := TObject.Create;
  FRegisteredSyntax := TDictionary<Integer, Integer>.Create();
  FData := TDictionary<Integer, PSyntaxData>.Create();
  FNextSyntax := 0;
end;

procedure TSyntaxManager.Leave;
begin
  TMonitor.Exit(FLock);
end;

function TSyntaxManager.RegisterSyntax(ASyntax: Integer; SyntaxProc: TDecoderProc; PRegsData: PRegistersData): Integer;
var
  PData: PSyntaxData;
begin
  Result := SME_REGISTER_SYNTAX_FAILED;
  if FNextSyntax < 0 then
    Init;
  if FNextSyntax > MAX_SYNTAX_DECODER then
    Exit(SME_EXCEEDED_MAX_SYNTAX_DECODER);
  try
    Enter;
    if FRegisteredSyntax.ContainsKey(ASyntax) then
      Exit(SME_SYNTAX_ALREADY_EXISTS);
    SyntaxDecoderArray[FNextSyntax] := SyntaxProc;
    FRegisteredSyntax.Add(ASyntax, FNextSyntax);
    PData := AllocMem(SizeOf(TSyntaxData));
    PData^.UserRegsData := PRegsData;
    if Assigned(PRegsData) then
    begin
      PData^.LegacyRegs[0] := PRegsData^.InvalidRegs;
      PData^.LegacyRegs[1] := PRegsData^.LegacyRegs8;
      PData^.LegacyRegs[2] := PRegsData^.LegacyRegs16;
      PData^.LegacyRegs[3] := PRegsData^.InvalidRegs;
      PData^.LegacyRegs[4] := PRegsData^.LegacyRegs32;
      PData^.LegacyRegs[5] := PRegsData^.InvalidRegs;
      PData^.LegacyRegs[6] := PRegsData^.InvalidRegs;
      PData^.LegacyRegs[7] := PRegsData^.InvalidRegs;
      PData^.LegacyRegs[8] := PRegsData^.LegacyRegs64;

      PData^.RegsName[00] := PRegsData^.InvalidRegs;
      PData^.RegsName[01] := PRegsData^.InvalidRegs;
      PData^.RegsName[02] := PRegsData^.FPURegs;
      PData^.RegsName[03] := PRegsData^.KRegs;
      PData^.RegsName[04] := PRegsData^.MMXRegs;
      PData^.RegsName[05] := PRegsData^.XMMRegs;
      PData^.RegsName[06] := PRegsData^.YMMRegs;
      PData^.RegsName[07] := PRegsData^.ZMMRegs;
      PData^.RegsName[08] := PRegsData^.CntrlRegs;
      PData^.RegsName[09] := PRegsData^.DbgRegs;
      PData^.RegsName[10] := PRegsData^.TableRegs;
      PData^.RegsName[11] := PRegsData^.BoundRegs;
      PData^.RegsName[12] := PRegsData^.SegRegs;
      PData^.RegsName[13] := PRegsData^.InvalidRegs;
      PData^.RegsName[14] := PRegsData^.InvalidRegs;
    end;
    FData.Add(ASyntax, PData);
    Result := FNextSyntax;
    Inc(FNextSyntax);
  finally
    Leave;
  end;
end;

initialization

SyntaxManager.RegisterSyntax(SX_UNIV_SYNTAX, UnivSyntax, GetUnivSyntaxData);
SyntaxManager.RegisterSyntax(SX_NIL_SYNTAX, NilSyntax, nil);

finalization

SyntaxManager.Die;

end.
