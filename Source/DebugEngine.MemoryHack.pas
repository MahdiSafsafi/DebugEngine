// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.MemoryHack
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.MemoryHack.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

unit DebugEngine.MemoryHack;

interface

{$I DebugEngine.inc}

uses WinApi.Windows;

type
  TStrType = (stUnknown, stPAnsiChar, stPChar, stAnsiString, stString);

  /// <summary> Get string type from address.
  /// </summary>
  /// <param name="Address"> The address where the string is located.
  /// </param>
  /// <param name="MinLength"> Minimum string length to accept.
  /// </param>
  /// <param name="MaxLength"> Maximum string length to accept.
  /// </param>
  /// <returns> Return string type <see cref="TStrType"/>.
  /// </returns>
  /// <remarks> The function will use <c>MinLength</c> and <c>MaxLength</c> only if the string type is <c>PAnsiChar</c> or <c>PChar</c>.
  /// </remarks>
function GetStrType(Address: Pointer; const MinLength: Integer = 0; const MaxLength: Integer = 0): TStrType;

/// <summary> Check if a pointer points out to a class.
/// </summary>
function IsPtrClass(Ptr: Pointer): Boolean;

implementation

type

  { From System.pas }
  StrRec = packed record
{$IF defined(CPU64BITS)}
    _Padding: Integer; // Make 16 byte align for payload..
{$ENDIF}
    codePage: Word;
    elemSize: Word;
    refCnt: Integer;
    length: Integer;
  end;

  PStrRec = ^StrRec;

function GetRawStrType(Ptr: Pointer; const MinLength: Integer = 0; const MaxLength: Integer = 0): TStrType;
var
  PStart: PByte;
  nZero: Integer;
  nb: Integer;
  SafeRange: Integer;
begin
  Result := stUnknown;
  if not Assigned(Ptr) then
    Exit;
  PStart := Ptr;
  nZero := 0;
  if MaxLength <= 0 then
    SafeRange := High(Integer) - 1
  else
    SafeRange := MaxLength;
  nb := 0; // Processed bytes.
  try
    // We're going to read from memory
    // Access violation error can occur !
    while True do
    begin
      if (PStart^ = $00) then
      begin
        if PWord(PStart)^ = $00 then
        begin
          if (PDWORD(PStart)^ and $00FFFFFF = $00) then
            Result := stPChar // Sure it's Unicode !
          else if nZero > 0 then
            Result := stPChar
          else
            Result := stPAnsiChar;
          Break;
        end;
        if nb > 1 then
        begin
          if nZero = 0 then
          begin
            Result := stPAnsiChar;
            Break;
          end;
        end;
        Inc(nZero);
      end;
      Inc(PStart);
      Inc(nb);
      if nb >= SafeRange then
        Break;
    end;
    if (Result <> stUnknown) then
    begin
      if Result = stPChar then
        nb := nb shr 1;
      if (MinLength > 0) and (nb < MinLength) then
        Result := stUnknown;
      if (MaxLength > 0) and (nb > MaxLength) then
        Result := stUnknown;
    end;
  except
    Result := stUnknown;
  end;
end;

function GetStrType(Address: Pointer; const MinLength: Integer = 0; const MaxLength: Integer = 0): TStrType;
var
  P: PByte;
  LPStrRec: PStrRec;
begin
  Result := stUnknown;
  try
    P := Address;
    LPStrRec := PStrRec(P - SizeOf(StrRec));
    // Check if is null terminated string.
    case LPStrRec^.elemSize of
      SizeOf(WideChar):
        begin
          if (LPStrRec^.codePage = DefaultUnicodeCodePage) and (PWord(P + (LPStrRec^.length * SizeOf(WideChar)))^ = $00) then
            Result := stString;
        end;
      SizeOf(AnsiChar):
        begin
          if (LPStrRec^.codePage = DefaultSystemCodePage) and (PByte(P + (LPStrRec^.length * SizeOf(AnsiChar)))^ = $00) then
            Result := stAnsiString;
        end;
    end;
    if (Result = stUnknown) then
      Result := GetRawStrType(Address, MinLength, MaxLength);
  except
    Exit(stUnknown);
  end;
end;

function IsPtrClass(Ptr: Pointer): Boolean;
var
  LClass: TClass;
  function GetLastClassParent(AClass: TClass): TClass;
  begin
    Result := AClass;
    try
      while True do
      begin
        if Result.ClassParent = nil then
          Exit;
        Result := Result.ClassParent;
      end;
    except
      Result := nil;
    end;
  end;
  function IsObjClass(AClass: TClass): Boolean;
  begin
    Result := (AClass.ClassInfo = TObject.ClassInfo) and (AClass.ClassNameIs(TObject.ClassName));
  end;

begin
  { We don't know if the pointer is a valid class
    Thus, we must handle any AV error and reject Ptr to be a class. }
  try
    LClass := GetLastClassParent(TClass(PPointer(Ptr)^));
    Result := LClass <> nil;
    if Result then
      Result := IsObjClass(LClass);
  except
    Exit(False);
  end;
end;

end.
