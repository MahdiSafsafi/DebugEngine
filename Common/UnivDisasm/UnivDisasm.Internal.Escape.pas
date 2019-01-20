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
// The Original Code is UnivDisasm.Internal.Escape.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Internal.Escape;

interface

{$I Config.inc}

uses UnivDisasm.Disasm;
procedure Decode_ESCAPE_TABLE_2_void(PInst: PInstruction);
procedure Decode_ESCAPE_TABLE_38_void(PInst: PInstruction);
procedure Decode_ESCAPE_TABLE_3A_void(PInst: PInstruction);
procedure Decode_ESCAPE_3DNow_F_void(PInst: PInstruction);
procedure Decode_ESCAPE_SSE5A_24_void(PInst: PInstruction);
procedure Decode_ESCAPE_SSE5A_25_void(PInst: PInstruction);
procedure Decode_ESCAPE_SSE5A_7A_void(PInst: PInstruction);
procedure Decode_ESCAPE_SSE5A_7B_void(PInst: PInstruction);
procedure Decode_ESCAPE_FPU_D8_void(PInst: PInstruction);
procedure Decode_ESCAPE_FPU_D9_void(PInst: PInstruction);
procedure Decode_ESCAPE_FPU_DA_void(PInst: PInstruction);
procedure Decode_ESCAPE_FPU_DB_void(PInst: PInstruction);
procedure Decode_ESCAPE_FPU_DC_void(PInst: PInstruction);
procedure Decode_ESCAPE_FPU_DD_void(PInst: PInstruction);
procedure Decode_ESCAPE_FPU_DE_void(PInst: PInstruction);
procedure Decode_ESCAPE_FPU_DF_void(PInst: PInstruction);

implementation

uses
  UnivDisasm.Internal.Common,
  UnivDisasm.Cnsts;

procedure Decode_ESCAPE_TABLE_2_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_2);
  TABLE_2[PInst^.NextInst^](PInst);
end;

procedure Decode_ESCAPE_TABLE_38_void(PInst: PInstruction);
begin
  // TABLE_38  void
  PInst^.SetTable(GF_TABLE_38);
  TABLE_38[PInst^.NextInst^](PInst);
end;

procedure Decode_ESCAPE_TABLE_3A_void(PInst: PInstruction);
begin
  // TABLE_3A  void
  PInst^.SetTable(GF_TABLE_3A);
  TABLE_3A[PInst^.NextInst^](PInst);
end;

procedure Decode_ESCAPE_3DNow_F_void(PInst: PInstruction);
begin
  // 3DNow_F  void
  PInst^.SetTable(GF_TABLE_3DNOW);
  DecodeModRm(PInst);
  TABLE_3DNOW[PInst^.NextInst^](PInst);
end;

procedure Decode_ESCAPE_SSE5A_24_void(PInst: PInstruction);
var
  Op: Byte;
begin
  // SSE5A  void
  PInst^.SetTable(GF_TABLE_SSE5_24);
  Op := PInst^.NextInst^; // Opcode3.
  if not PInst^.Next() then
    Exit;
  PInst^.InternalData.DRex := True;
  DecodeModRm(PInst);
  TABLE_SSE5A24[Op](PInst);
end;

procedure Decode_ESCAPE_SSE5A_25_void(PInst: PInstruction);
var
  Op: Byte;
begin
  // SSE5A  void
  PInst^.SetTable(GF_TABLE_SSE5_25);
  Op := PInst^.NextInst^; // Opcode3.
  if not PInst^.Next() then
    Exit;
  PInst^.InternalData.DRex := True;
  DecodeModRm(PInst);
  TABLE_SSE5A25[Op](PInst);
end;

procedure Decode_ESCAPE_SSE5A_7A_void(PInst: PInstruction);
var
  Op: Byte;
begin
  // SSE5A  void
  PInst^.SetTable(GF_TABLE_SSE5_7A);
  Op := PInst^.NextInst^; // Opcode3.
  if not PInst^.Next() then
    Exit;
  PInst^.InternalData.DRex := True;
  DecodeModRm(PInst);
  TABLE_SSE5A7A[Op](PInst);
end;

procedure Decode_ESCAPE_SSE5A_7B_void(PInst: PInstruction);
var
  Op: Byte;
begin
  // SSE5A  void
  PInst^.SetTable(GF_TABLE_SSE5_7B);
  Op := PInst^.NextInst^; // Opcode3.
  if not PInst^.Next() then
    Exit;
  PInst^.InternalData.DRex := True;
  DecodeModRm(PInst);
  TABLE_SSE5A7B[Op](PInst);
end;

procedure Decode_ESCAPE_FPU_D8_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_FPU_D8);
  DecodeModRm(PInst);
  TABLE_FPU0[PInst^.ModRm.Value](PInst);
end;

procedure Decode_ESCAPE_FPU_D9_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_FPU_D9);
  DecodeModRm(PInst);
  TABLE_FPU1[PInst^.ModRm.Value](PInst);
end;

procedure Decode_ESCAPE_FPU_DA_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_FPU_DA);
  DecodeModRm(PInst);
  TABLE_FPU2[PInst^.ModRm.Value](PInst);
end;

procedure Decode_ESCAPE_FPU_DB_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_FPU_DB);
  DecodeModRm(PInst);
  TABLE_FPU3[PInst^.ModRm.Value](PInst);
end;

procedure Decode_ESCAPE_FPU_DC_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_FPU_DC);
  DecodeModRm(PInst);
  TABLE_FPU4[PInst^.ModRm.Value](PInst);
end;

procedure Decode_ESCAPE_FPU_DD_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_FPU_DD);
  DecodeModRm(PInst);
  TABLE_FPU5[PInst^.ModRm.Value](PInst);
end;

procedure Decode_ESCAPE_FPU_DE_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_FPU_DE);
  DecodeModRm(PInst);
  TABLE_FPU6[PInst^.ModRm.Value](PInst);
end;

procedure Decode_ESCAPE_FPU_DF_void(PInst: PInstruction);
begin
  PInst^.SetTable(GF_TABLE_FPU_DF);
  DecodeModRm(PInst);
  TABLE_FPU7[PInst^.ModRm.Value](PInst);
end;

end.
