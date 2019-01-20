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
// The Original Code is UnivDisasm.Utils.pas
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2015-2019 Mahdi Safsafi.
// All Rights Reserved.
// *************************************************************************** //
//

unit UnivDisasm.Utils;

interface

{$I Config.inc}

uses UnivDisasm.Disasm;

function fIntToHex(Value: Integer; Digits: Integer; Buffer: PUnivChar): Integer; overload;
function fIntToHex(Value: Int64; Digits: Integer; Buffer: PUnivChar): Integer; overload;
function fIntToHex(Value: UInt64; Digits: Integer; Buffer: PUnivChar): Integer; overload;
procedure fMove(const Source; var Dest; Count: NativeInt); {$IF DEFINED(PUREPASCAL) AND DEFINED(MustInline)}inline; {$ENDIF}

implementation

const
  TwoHexLookup: packed array [0 .. 255] of array [1 .. 2] of UnivChar = ( //
    '00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '0a', '0b', '0c', '0d', '0e', '0f', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '1a', '1b', '1c', '1d', '1e', '1f', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '2a', '2b', '2c', '2d', '2e', '2f', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '3a',
    '3b', '3c', '3d', '3e', '3f', '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '4a', '4b', '4c', '4d', '4e', '4f', '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '5a', '5b', '5c', '5d', '5e', '5f', '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', '6a', '6b', '6c', '6d', '6e', '6f', '70', '71', '72', '73', '74', '75',
    '76', '77', '78', '79', '7a', '7b', '7c', '7d', '7e', '7f', '80', '81', '82', '83', '84', '85', '86', '87', '88', '89', '8a', '8b', '8c', '8d', '8e', '8f', '90', '91', '92', '93', '94', '95', '96', '97', '98', '99', '9a', '9b', '9c', '9d', '9e', '9f', 'a0', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'aa', 'ab', 'ac', 'ad', 'ae', 'af', 'b0',
    'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 'ba', 'bb', 'bc', 'bd', 'be', 'bf', 'c0', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'ca', 'cb', 'cc', 'cd', 'ce', 'cf', 'd0', 'd1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'da', 'db', 'dc', 'dd', 'de', 'df', 'e0', 'e1', 'e2', 'e3', 'e4', 'e5', 'e6', 'e7', 'e8', 'e9', 'ea', 'eb',
    'ec', 'ed', 'ee', 'ef', 'f0', 'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9', 'fa', 'fb', 'fc', 'fd', 'fe', 'ff');

function _fIntToHex(Value: UInt64; Digits: Integer; Pc: PUnivChar): Integer;
var
  I32: Integer;
  I, J: UInt64;
  P: Integer;
  NewLen: Integer;
begin
  NewLen := 1;
  I := Value shr 4;
  while I > 0 do
  begin
    Inc(NewLen);
    I := I shr 4;
  end;
  if Digits > NewLen then
  begin
    Result := Digits;
    for I32 := 0 to (Digits - NewLen) - 1 do
      Pc[I32] := '0';
    P := Digits - NewLen;
  end
  else
  begin
    Result := NewLen;
    P := 0;
  end;
  I := Value;
  while NewLen > 2 do
  begin
    J := I and $FF;
    I := I shr 8;
    Dec(NewLen, 2);
    Pc[P + NewLen] := TwoHexLookup[J][1];
    Pc[P + NewLen + 1] := TwoHexLookup[J][2];
  end;
  if NewLen = 2 then
  begin
    Pc[P] := TwoHexLookup[I][1];
    Pc[P + 1] := TwoHexLookup[I][2];
  end
  else
    Pc[P] := TwoHexLookup[I][2];
end;

function fIntToHex(Value: Integer; Digits: Integer; Buffer: PUnivChar): Integer;
begin
  Result := _fIntToHex(Cardinal(Value), Digits, Buffer);
end;

function fIntToHex(Value: Int64; Digits: Integer; Buffer: PUnivChar): Integer;
begin
  Result := _fIntToHex(Value, Digits, Buffer);
end;

function fIntToHex(Value: UInt64; Digits: Integer; Buffer: PUnivChar): Integer;
begin
  Result := _fIntToHex(Value, Digits, Buffer);
end;

procedure fMove(const Source; var Dest; Count: NativeInt);
{$IFDEF PUREPASCAL}
begin
  Move(Source, Dest, Count);
end;
{$ELSE !PUREPASCAL}
asm
  {$IFDEF CPUX86}
  { EAX = Source
  ECX = Count
  EDX = Dest }
  push ebx
  cmp ecx,8
  jle @less_eq_8
  jmp @large_8
@less_eq_8:
  shl ecx,2
  add ecx,offset @@jmp_address_data
  jmp [ecx]
  jmp @end

@mov1: { Move one byte }
  mov eax,[eax]
  mov byte[edx],al
  jmp @end

@mov2: { Move two bytes }
  mov eax,[eax]
  mov word[edx],ax
  jmp @end

@mov3: { Move 3 Bytes }
  mov eax,[eax]
  mov byte[edx],al
  shr eax,8
  mov word[edx+1],ax
  jmp @end

@mov4:    { Move dword (4 Bytes) }
  mov eax,[eax]
  mov dword[edx],eax
  jmp @end

@mov5:   { Move 5 Bytes }
  mov ebx,[eax+4]
  mov eax,[eax]
  mov dword[edx],eax
  mov byte[edx+4],bl
  jmp @end

@mov6: { Move 6 Bytes }
  mov ebx,[eax+4]
  mov eax,[eax]
  mov dword[edx],eax
  mov word[edx+4],bx
  jmp @end

@mov7:  { Move 7 Bytes }
  mov ebx,[eax+4]
  mov eax,[eax]
  mov dword[edx],eax
  mov word[edx+4],bx
  shr ebx,16
  mov byte[edx+6],bl
  jmp @end

@mov8:   { Move qword (8 Bytes) }
  movq mm0,qword[eax]
  movq qword[edx],mm0
  emms
  jmp @end

@large_8:{ Bigger than 8 bytes ! }
  movq mm0,qword[eax+ecx*1-8]
  movq qword[edx+ecx*1-8],mm0
  add ecx,-8
  cmp ecx,8
  jge @large_8
  emms
  shl ecx,2
  add ecx,offset @@jmp_address_data
  jmp [ecx]  // Process the rest !
  jmp @end

@@jmp_address_data:
  dd @end
  dd @mov1
  dd @mov2
  dd @mov3
  dd @mov4
  dd @mov5
  dd @mov6
  dd @mov7
  dd @mov8
  dd @end
@end:
  pop ebx
  {$ELSE !CPUX86}
  { RCX = Source
  R8 = Count
  RDX = Dest }
  push r9
  and r8,$FFFFFFFF
  cmp r8,8
  jle @less_eq_8
  jmp @large_8
@less_eq_8:
  shl r8,3
  mov r9,offset @@jmp_address_data
  add r9,r8
  jmp [r9]
  jmp @end

@mov1: { Move one byte }
  mov rcx,qword[rcx]
  mov byte[rdx],cl
  jmp @end

@mov2: { Move two bytes }
  mov rcx,qword[rcx]
  mov word[rdx],cx
  jmp @end

@mov3: { Move 3 bytes }
  mov rcx,qword[rcx]
  mov byte[rdx],cl
  shr ecx,8
  mov word[rdx+1],cx
  jmp @end

@mov4: { Move 4 bytes }
  mov rcx,qword[rcx]
  mov dword[rdx],ecx
  jmp @end

@mov5: { Move 5 bytes }
  mov rcx,qword[rcx]
  mov dword[rdx],ecx
  shr rcx,32
  mov byte[rdx+4],cl
  jmp @end

@mov6: { Move 6 bytes }
  mov rcx,qword[rcx]
  mov dword[rdx],ecx
  shr rcx,32
  mov word[rdx+4],cx
  jmp @end

@mov7: { Move 7 bytes }
  mov rcx,qword[rcx]
  mov dword[rdx],ecx
  shr rcx,32
  mov word[rdx+4],cx
  shr rcx,16
  mov byte[rdx+6],cl
  jmp @end

@mov8: { Move 8 bytes }
  mov rcx,qword[rcx]
  mov qword[rdx],rcx
  jmp @end

@large_8:
  mov r9,qword[rcx+r8*1-8]
  mov qword[rdx+r8*1-8],r9
  add r8,-8
  cmp r8,8
  jge @large_8
  shl r8,3
  mov r9,offset @@jmp_address_data
  add r9,r8
  jmp [r9]  // Process the rest !

@@jmp_address_data:
  dq @end
  dq @mov1
  dq @mov2
  dq @mov3
  dq @mov4
  dq @mov5
  dq @mov6
  dq @mov7
  dq @mov8
  dq @end

@end:
  pop r9
  {$ENDIF CPUX86}
end;
{$ENDIF PUREPASCAL}

end.
