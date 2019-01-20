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
// The Original Code is UnivDisasm.Syntax.NilSyntax.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Syntax.NilSyntax;

interface

{$I Config.inc}

uses
  UnivDisasm.Disasm;

procedure NilSyntax(PInst: PInstruction);

const
  SX_NIL_SYNTAX = $01;

implementation

uses UnivDisasm.Cnsts;

procedure NilSyntax(PInst: PInstruction);
begin
  { Nothing !!! }
  FillChar(PInst^.InstStr, 4, #$00);
end;

end.
