// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.PeUtils
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.PeUtils.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

unit DebugEngine.PeUtils;

interface

{$I DebugEngine.inc}

uses
  WinApi.Windows,
  System.SysUtils;

type
  TMachineType = (mtUnknown, mt32, mt64);

function PeMapImageDosHeader(BaseAddress: Pointer): PImageDosHeader;
function PeMapImageNtHeaders32(BaseAddress: PByte): PImageNtHeaders32;
function PeMapImageNtHeaders64(BaseAddress: PByte): PImageNtHeaders64;
function PeMapImageNtHeaders(BaseAddress: PByte): PImageNtHeaders;
function PeMapImageFileHeader(NtHeadersX: Pointer): PImageFileHeader;
function PeMapImageMachine(PFileHeader: PImageFileHeader): TMachineType;
function PeMapImageSectionHeader(NtHeadersX: Pointer): PImageSectionHeader;
function PeFindSection(NtHeadersX: Pointer; const Section: String): PImageSectionHeader;
function FindDebugSection32(NtHeaders: PImageNtHeaders32): PImageSectionHeader;
function FindDebugSection64(NtHeaders: PImageNtHeaders64): PImageSectionHeader;

implementation

function PeMapImageDosHeader(BaseAddress: Pointer): PImageDosHeader;
begin
  Result := BaseAddress;
end;

function PeMapImageNtHeaders32(BaseAddress: PByte): PImageNtHeaders32;
begin
  Result := Pointer(BaseAddress + PeMapImageDosHeader(BaseAddress)^._lfanew);
  if Result^.Signature <> IMAGE_NT_SIGNATURE then
    Result := nil;
end;

function PeMapImageNtHeaders64(BaseAddress: PByte): PImageNtHeaders64;
begin
  Result := Pointer(BaseAddress + PeMapImageDosHeader(BaseAddress)^._lfanew);
  if Result^.Signature <> IMAGE_NT_SIGNATURE then
    Result := nil;
end;

function PeMapImageNtHeaders(BaseAddress: PByte): PImageNtHeaders;
begin
{$IFDEF CPUX86}
  Result := PeMapImageNtHeaders32(BaseAddress);
{$ELSE !CPUX86}
  Result := PeMapImageNtHeaders64(BaseAddress);
{$ENDIF CPUX86}
end;

function PeMapImageFileHeader(NtHeadersX: Pointer): PImageFileHeader;
begin
  { NtHeadersX could be PImageNtHeaders32 or PImageNtHeaders64 !
    In both case they share the same FileHeader location. }
  Result := @PImageNtHeaders32(NtHeadersX)^.FileHeader;
end;

function PeMapImageMachine(PFileHeader: PImageFileHeader): TMachineType;
begin
  case PFileHeader^.Machine of
    IMAGE_FILE_MACHINE_I386: Exit(mt32);
    IMAGE_FILE_MACHINE_AMD64: Exit(mt64);
  else Result := mtUnknown;
  end;
end;

function PeMapImageSectionHeader(NtHeadersX: Pointer): PImageSectionHeader;
begin
  Result := Pointer(PByte(@PImageNtHeaders(NtHeadersX)^.OptionalHeader) + PImageNtHeaders(NtHeadersX)^.FileHeader.SizeOfOptionalHeader);
end;

function PeFindSection(NtHeadersX: Pointer; const Section: String): PImageSectionHeader;
var
  nSections: Integer;
  LSectionName: AnsiString;
begin
  nSections := PImageNtHeaders32(NtHeadersX)^.FileHeader.NumberOfSections;
  Result := PeMapImageSectionHeader(NtHeadersX);
  while nSections > 0 do
  begin
    LSectionName := AnsiString(PAnsiChar(@Result^.Name[0]));
    if SameText(Section, String(LSectionName)) then
      Exit;
    Inc(Result);
    Dec(nSections);
  end;
  Result := nil;
end;

function FindDebugSection32(NtHeaders: PImageNtHeaders32): PImageSectionHeader;
var
  I: Integer;
  VA: Cardinal;
begin
  Result := PeMapImageSectionHeader(NtHeaders);
  VA := NtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress;
  for I := 1 to NtHeaders^.FileHeader.NumberOfSections do
  begin
    if Result^.VirtualAddress = VA then
      Exit;
    Inc(Result);
  end;
  Result := nil;
end;

function FindDebugSection64(NtHeaders: PImageNtHeaders64): PImageSectionHeader;
var
  I: Integer;
  VA: Cardinal;
begin
  Result := PeMapImageSectionHeader(NtHeaders);
  VA := NtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress;
  for I := 1 to NtHeaders^.FileHeader.NumberOfSections do
  begin
    if Result^.VirtualAddress = VA then
      Exit;
    Inc(Result);
  end;
  Result := nil;
end;

end.
