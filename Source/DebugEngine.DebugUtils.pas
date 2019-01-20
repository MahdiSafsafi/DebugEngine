// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.DebugUtils
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.DebugUtils.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

unit DebugEngine.DebugUtils;

interface

{$I DebugEngine.inc}

uses
  WinApi.Windows,
  WinApi.ImageHlp,
  System.Classes,
  System.SysUtils;

/// <summary>  Insert debug information into PE.
/// </summary>
/// <param name="FileName"> PE file.
/// </param>
/// <param name="SMapFileName"> Path to smap file.
/// </param>
/// <param name="SectionFlavor">
/// <c>If SectionFlavor = True</c>, the function will try to insert debug info into a PE section first,
/// if it failed to insert into section, the function will insert debug info into resource.
/// <para></para>
/// <c>If SectionFlavor = False</c>, the function will insert debug info directly into resource
/// without trying to insert into PE section.
/// </param>
/// <returns> If the function succeeds, the return value is True.
/// </returns>
function InsertDebugInfo(const FileName, SMapFileName: string; SectionFlavor: Boolean): Boolean;

/// <summary> Remove Delphi debug information from PE.
/// </summary>
/// <param name="FileName"> PE file name.
/// </param>
/// <param name="DebugStream"> Optional stream that will receive debug data. If assigned, the function will copy debug's data first into
/// that stream, than it removes debug data from PE.
/// <para></para>
/// Usually, you need to use this param only if you want to restore debug data later.
/// </param>
/// <returns> If the function succeeds, the return value is True.
/// </returns>
/// <remarks> Before inserting debug data, the function inserts first an internal struct data into the stream.
/// <para></para>
/// To obtain only delphi debug data. Seek the stream with the value returned by <see cref="SizeOfInternalDebugInfoData"/>.
/// </remarks>
function RemoveDebugInfo(const FileName: string; DebugStream: TStream): Boolean;

/// <summary> Restore Delphi debug information.
/// </summary>
/// <param name="FileName"> PE file name.
/// </param>
/// <param name="DebugStream"> Stream that holds Delphi debug data.
/// </param>
/// <returns> If the function succeeds, the return value is True.
/// </returns>
function RestoreDebugInfo(const FileName: string; DebugStream: TStream): Boolean;

/// <summary> Return size of internal data that was inserted into stream by
/// <see cref="RemoveDebugInfo"/> function.
/// </summary>
function SizeOfInternalDebugInfoData: Integer;

implementation

uses
  DebugEngine.DebugInfo,
  DebugEngine.PeUtils;

{ Internal data used by RemoveDebugInfo &  RestoreDebugInfo }
type
  TInternalDebugInfoData = record
    DebugVirtualAddress: DWORD;
    PointerToRawData: DWORD;
    NumberOfDebugDirectories: DWORD;
  end;

  PInternalDebugInfoData = ^TInternalDebugInfoData;

function ALIGN(Value, Alignment: Cardinal): Cardinal;
begin
  Result := Value;
  if Value mod Alignment = 0 then
    Exit;
  Result := (Result + Alignment - 1) div Alignment * Alignment;
end;

function SizeOfInternalDebugInfoData: Integer;
begin
  { use this function to skip Internal data,
    In case you want to dump original debug data
    eg:
    DebugStream.Seek(SizeOfInternalDebugInfoData,soBeginning); }

  Result := SizeOf(TInternalDebugInfoData);
end;

function InsertDebugInfo(const FileName, SMapFileName: string; SectionFlavor: Boolean): Boolean;
const
  SafeArea = $FFFF; // Just for alignment.
var
  LDebugStream: TMemoryStream;
  LDelphiDebugInfo: TMemoryStream;
  FileHandle: THandle;
  MapObj: THandle;
  FS: Cardinal;
  FP: Cardinal;
  MapAddress: Pointer;
  hUpdate: THandle;
  function InsertDebugInfoIntoSection32: Cardinal;
  var
    NtHeaders: PImageNtHeaders32;
    FirstSection: PImageSectionHeader;
    LastSection: PImageSectionHeader;
    DebugSection: PImageSectionHeader;
    DebugSize: Cardinal;
    LSectionAlignment: DWORD;
    LFileAlignment: DWORD;
    SectionName: AnsiString;
  begin
    NtHeaders := PeMapImageNtHeaders32(MapAddress);
    DebugSize := LDebugStream.Size;
    SectionName := AnsiString(SDebugSection); // Ansi format.

    with NtHeaders^.OptionalHeader do
    begin
      LFileAlignment := FileAlignment;
      LSectionAlignment := SectionAlignment;
    end;
    FirstSection := PeMapImageSectionHeader(NtHeaders);
    LastSection := FirstSection;
    Inc(LastSection, NtHeaders^.FileHeader.NumberOfSections - 1);
    DebugSection := LastSection;
    Inc(DebugSection);

    { Check if we are going to override adjacent section data. }
    if (NativeUInt(DebugSection) + SizeOf(TImageSectionHeader)) > (NativeUInt(MapAddress) + FirstSection^.PointerToRawData) then
      Exit(0);

    { New section => Increase NumberOfSections. }
    Inc(NtHeaders^.FileHeader.NumberOfSections);

    { Initialize DebugSection }
    ZeroMemory(DebugSection, SizeOf(TImageSectionHeader));
    { Set section name. }
    Move(PAnsiChar(@SectionName[1])^, DebugSection^.Name, IMAGE_SIZEOF_SHORT_NAME - 1);

    { VirtualSize = DebugSize }
    DebugSection^.Misc.VirtualSize := DebugSize;
    { VirtualAddress = Previous section's VirtualAddress + it's aligned size. }
    DebugSection^.VirtualAddress := LastSection^.VirtualAddress + ALIGN(LastSection^.Misc.VirtualSize, LSectionAlignment);

    { Set DebugSection size }
    DebugSection^.SizeOfRawData := ALIGN(DebugSize, LFileAlignment);
    { PointerToRawData = PointerToRawData of the previous section + size of its aligned RawData. }
    DebugSection^.PointerToRawData := (LastSection^.PointerToRawData + ALIGN(LastSection^.SizeOfRawData, LFileAlignment));

    DebugSection^.Characteristics := IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_INITIALIZED_DATA;

    { Copy debug data to it's section in the PE. }
    Move(LDebugStream.Memory^, Pointer(NativeUInt(MapAddress) + DebugSection^.PointerToRawData)^, DebugSize);
    { Update SizeOfImage }
    Inc(NtHeaders^.OptionalHeader.SizeOfImage, DebugSection^.Misc.VirtualSize);
    { Update SizeOfInitializedData }
    Inc(NtHeaders^.OptionalHeader.SizeOfInitializedData, DebugSection^.SizeOfRawData);

    { Return file pointer }
    Result := DebugSection^.PointerToRawData + DebugSection^.SizeOfRawData;
  end;
  function InsertDebugInfoIntoSection64: Cardinal;
  var
    NtHeaders: PImageNtHeaders64;
    FirstSection: PImageSectionHeader;
    LastSection: PImageSectionHeader;
    DebugSection: PImageSectionHeader;
    DebugSize: Cardinal;
    LSectionAlignment: DWORD;
    LFileAlignment: DWORD;
    SectionName: AnsiString;
  begin
    NtHeaders := PeMapImageNtHeaders64(MapAddress);
    DebugSize := LDebugStream.Size;
    SectionName := AnsiString(SDebugSection); // Ansi format.

    with NtHeaders^.OptionalHeader do
    begin
      LFileAlignment := FileAlignment;
      LSectionAlignment := SectionAlignment;
    end;
    FirstSection := PeMapImageSectionHeader(NtHeaders);
    LastSection := FirstSection;
    Inc(LastSection, NtHeaders^.FileHeader.NumberOfSections - 1);
    DebugSection := LastSection;
    Inc(DebugSection);

    { Check if we are going to override adjacent section data. }
    if (NativeUInt(DebugSection) + SizeOf(TImageSectionHeader)) > (NativeUInt(MapAddress) + FirstSection^.PointerToRawData) then
      Exit(0);

    { New section => Increase NumberOfSections. }
    Inc(NtHeaders^.FileHeader.NumberOfSections);

    { Initialize DebugSection }
    ZeroMemory(DebugSection, SizeOf(TImageSectionHeader));
    { Set section name. }
    Move(PAnsiChar(@SectionName[1])^, DebugSection^.Name, IMAGE_SIZEOF_SHORT_NAME - 1);

    { VirtualSize = DebugSize }
    DebugSection^.Misc.VirtualSize := DebugSize;
    { VirtualAddress = Previous section's VirtualAddress + it's aligned size. }
    DebugSection^.VirtualAddress := LastSection^.VirtualAddress + ALIGN(LastSection^.Misc.VirtualSize, LSectionAlignment);

    { Set DebugSection size }
    DebugSection^.SizeOfRawData := ALIGN(DebugSize, LFileAlignment);
    { PointerToRawData = PointerToRawData of the previous section + size of its aligned RawData. }
    DebugSection^.PointerToRawData := (LastSection^.PointerToRawData + ALIGN(LastSection^.SizeOfRawData, LFileAlignment));

    DebugSection^.Characteristics := IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_INITIALIZED_DATA;

    { Copy debug data to it's section in the PE. }
    Move(LDebugStream.Memory^, Pointer(NativeUInt(MapAddress) + DebugSection^.PointerToRawData)^, DebugSize);
    { Update SizeOfImage }
    Inc(NtHeaders^.OptionalHeader.SizeOfImage, DebugSection^.Misc.VirtualSize);
    { Update SizeOfInitializedData }
    Inc(NtHeaders^.OptionalHeader.SizeOfInitializedData, DebugSection^.SizeOfRawData);

    { Return file pointer }
    Result := DebugSection^.PointerToRawData + DebugSection^.SizeOfRawData;
  end;

begin
  LDebugStream := TMemoryStream.Create;
  LDebugStream.LoadFromFile(SMapFileName);
  FP := 0;

  if SectionFlavor then
  begin
    FileHandle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if FileHandle <> INVALID_HANDLE_VALUE then
    begin
      FS := GetFileSize(FileHandle, nil);
      FS := FS + LDebugStream.Size + SafeArea;
      MapObj := CreateFileMapping(FileHandle, nil, PAGE_READWRITE, 0, FS, nil);
      if MapObj <> 0 then
      begin
        MapAddress := MapViewOfFile(MapObj, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
        if Assigned(MapAddress) then
        begin
          case PeMapImageMachine(PeMapImageFileHeader(PeMapImageNtHeaders32(MapAddress))) of
            mt32: FP := InsertDebugInfoIntoSection32;
            mt64: FP := InsertDebugInfoIntoSection64;
          end;
          UnmapViewOfFile(MapAddress);
        end;
        CloseHandle(MapObj);
      end;
      if FP <> 00 then
      begin
        SetFilePointer(FileHandle, FP, nil, FILE_BEGIN);
        SetEndOfFile(FileHandle);
      end;
      CloseHandle(FileHandle);
    end;
  end;
  Result := FP <> 0;
  if not Result then
  begin
    LDelphiDebugInfo := TMemoryStream.Create;
    try
      RemoveDebugInfo(FileName, LDelphiDebugInfo);
      hUpdate := BeginUpdateResource(PChar(FileName), False);
      if hUpdate <> 0 then
      begin
        if UpdateResource(hUpdate, SMapResType, SMapResName, 0, LDebugStream.Memory, LDebugStream.Size) then
          Result := EndUpdateResource(hUpdate, False);
      end;
      LDelphiDebugInfo.Seek(0, soBeginning);
      RestoreDebugInfo(FileName, LDelphiDebugInfo);
    finally
      LDelphiDebugInfo.Free;
    end;
  end;
  LDebugStream.Free;
end;

function RemoveDebugInfo(const FileName: string; DebugStream: TStream): Boolean;
var
  FileHandle: THandle;
  MapObj: THandle;
  MapAddress: Pointer;
  DebugSection: PImageSectionHeader;
  PreviousSection: PImageSectionHeader;
  Machine: TMachineType;
  NtHeaders32: PImageNtHeaders32;
  NtHeaders64: PImageNtHeaders64;
  NtHeaders: PImageNtHeaders;
  DebugVA: Pointer;
  // LSectionAlignment: DWORD;
  // LFileAlignment: DWORD;
  pSizeOfImage: PDWORD;
  DebugDirectoryEntry: PImageDataDirectory;
  FP: Cardinal;
  InternalDebugData: TInternalDebugInfoData;
begin
  FP := 0;
  NtHeaders32 := nil;
  NtHeaders64 := nil;
  DebugSection := nil;
  Result := False;

  { Initialize datas and make the compiler happy :) }
  pSizeOfImage := nil;
  DebugDirectoryEntry := nil;

  FileHandle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    MapObj := CreateFileMapping(FileHandle, nil, PAGE_READWRITE, 0, 0, nil);
    if MapObj <> 0 then
    begin
      MapAddress := MapViewOfFile(MapObj, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
      if Assigned(MapAddress) then
      begin
        Machine := PeMapImageMachine(PeMapImageFileHeader(PeMapImageNtHeaders32(MapAddress)));
        case Machine of
          mt32:
            begin
              NtHeaders32 := PeMapImageNtHeaders32(MapAddress);
              with NtHeaders32^.OptionalHeader do
              begin
                // LSectionAlignment := SectionAlignment;
                // LFileAlignment := FileAlignment;
                pSizeOfImage := @SizeOfImage;
                DebugDirectoryEntry := @DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG];
                DebugSection := FindDebugSection32(NtHeaders32);
              end;
            end;
          mt64:
            begin
              NtHeaders64 := PeMapImageNtHeaders64(MapAddress);
              with NtHeaders64^.OptionalHeader do
              begin
                // LSectionAlignment := SectionAlignment;
                // LFileAlignment := FileAlignment;
                pSizeOfImage := @SizeOfImage;
                DebugDirectoryEntry := @DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG];
                DebugSection := FindDebugSection64(NtHeaders64);
              end;
            end;
        end;
        if Assigned(DebugSection) then
        begin
          if Assigned(NtHeaders32) then
            NtHeaders := Pointer(NtHeaders32)
          else
            NtHeaders := Pointer(NtHeaders64);

          DebugVA := ImageRvaToVa(Pointer(NtHeaders), MapAddress, DebugSection^.VirtualAddress, nil);
          { Remove last section = Debug section. }
          Dec(NtHeaders^.FileHeader.NumberOfSections);

          PreviousSection := DebugSection;
          Dec(PreviousSection); // Resource section.

          with PreviousSection^ do
          begin
            { No need to align => Everything is aligned correctly. }
            pSizeOfImage^ := VirtualAddress + Misc.VirtualSize;
            FP := PointerToRawData + SizeOfRawData;
          end;

          { Save debug directory data. }
          if Assigned(DebugStream) then
          begin
            { ===> IMPORTANT <===
              1-From MSDN: DebugDirectoryEntry.Size is the size of all debug directories in bytes.
              However, Delphi does not follow this rule, it treats DebugDirectoryEntry.Size
              as it was the number of debug directories !

              2-Also, it seems that Delphi adds a null DWORD value after each debug directory.
              So we use an InternalDebugData to store old VirtualAddress,PointerToRawData and Size of debug section.
              We use the Size to set the number of directories and
              we use the VirtualAddress,PointerToRawData to apply
              an offset to the debug directory's raw-data. }

            with InternalDebugData do
            begin
              DebugVirtualAddress := DebugDirectoryEntry^.VirtualAddress;
              PointerToRawData := DebugSection^.PointerToRawData;
              NumberOfDebugDirectories := DebugDirectoryEntry^.Size;
            end;
            DebugStream.Write(InternalDebugData, SizeOf(InternalDebugData));
            DebugStream.Write(DebugVA^, DebugSection^.Misc.VirtualSize);
          end;

          { Remove debug entry directory }
          DebugDirectoryEntry^.VirtualAddress := 0;
          DebugDirectoryEntry^.Size := 0;

        end;
        UnmapViewOfFile(MapAddress);
      end;
      CloseHandle(MapObj);
    end;
    if FP <> 0 then
    begin
      Result := True;
      SetFilePointer(FileHandle, FP, nil, FILE_BEGIN);
      SetEndOfFile(FileHandle);
    end;
    CloseHandle(FileHandle);
  end;
end;

function RestoreDebugInfo(const FileName: string; DebugStream: TStream): Boolean;
const
  SafeArea = $FFFF;
var
  FileHandle: THandle;
  MapObj: THandle;
  MapAddress: Pointer;
  NtHeaders32: PImageNtHeaders32;
  NtHeaders64: PImageNtHeaders64;
  NtHeaders: PImageNtHeaders; // common.
  PreviousSection: PImageSectionHeader;
  DebugSection: PImageSectionHeader;
  Machine: TMachineType;
  LSectionAlignment: DWORD;
  LFileAlignment: DWORD;
  pSizeOfImage: PDWORD;
  DebugDirectoryEntry: PImageDataDirectory;
  DebugDirectory: PImageDebugDirectory;
  P: Pointer;
  VA: Cardinal; // for optimization.
  FS: Cardinal;
  FP: Cardinal;
  InternalDebugData: TInternalDebugInfoData;
  I: Integer;
begin
  Result := False;
  if (not Assigned(DebugStream)) or (DebugStream.Size = 0) then
    Exit;
  FP := 0;
  NtHeaders32 := nil; // Must be Initialized.
  NtHeaders64 := nil; // Must be Initialized.

  { Initialize datas and make the compiler happy :) }
  pSizeOfImage := nil;
  DebugDirectoryEntry := nil;
  LSectionAlignment := 0;
  LFileAlignment := 0;

  DebugStream.Read(InternalDebugData, SizeOf(InternalDebugData));

  FileHandle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    FS := GetFileSize(FileHandle, nil);
    FS := FS + DebugStream.Size + SafeArea;
    MapObj := CreateFileMapping(FileHandle, nil, PAGE_READWRITE, 0, FS, nil);
    if MapObj <> 0 then
    begin
      MapAddress := MapViewOfFile(MapObj, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
      if Assigned(MapAddress) then
      begin
        Machine := PeMapImageMachine(PeMapImageFileHeader(PeMapImageNtHeaders32(MapAddress)));
        case Machine of
          mt32:
            begin
              NtHeaders32 := PeMapImageNtHeaders32(MapAddress);
              with NtHeaders32^.OptionalHeader do
              begin
                LSectionAlignment := SectionAlignment;
                LFileAlignment := FileAlignment;
                pSizeOfImage := @SizeOfImage;
                DebugDirectoryEntry := @DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG];
              end;
            end;
          mt64:
            begin
              NtHeaders64 := PeMapImageNtHeaders64(MapAddress);
              with NtHeaders64^.OptionalHeader do
              begin
                LSectionAlignment := SectionAlignment;
                LFileAlignment := FileAlignment;
                pSizeOfImage := @SizeOfImage;
                DebugDirectoryEntry := @DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG];
              end;
            end;
        end;

        if Assigned(NtHeaders32) then
          NtHeaders := Pointer(NtHeaders32)
        else
          NtHeaders := Pointer(NtHeaders64);
        if Assigned(NtHeaders) then
        // Just in case machine <> mt32|mt64.
        begin
          { Usually PreviousSection = resource section. }
          PreviousSection := PeMapImageSectionHeader(NtHeaders);
          Inc(PreviousSection, NtHeaders^.FileHeader.NumberOfSections - 1);
          { Original debug section. }
          DebugSection := PreviousSection;
          Inc(DebugSection);
          { Increase NumberOfSections }
          Inc(NtHeaders^.FileHeader.NumberOfSections);
          { VirtualSize = DebugStream.Size }
          DebugSection^.Misc.VirtualSize := DebugStream.Size;

          with PreviousSection^ do
          begin
            { Debug's VirtualAddress = Previous section's VirtualAddress + its aligned VirtualSize. }
            VA := ALIGN(VirtualAddress + Misc.VirtualSize, LSectionAlignment);
            DebugSection^.VirtualAddress := VA;
            { PointerToRawData = Previous section's PointerToRawData + it's RawData size. }
            DebugSection^.PointerToRawData := PointerToRawData + SizeOfRawData;
          end;
          { SizeOfRawData must be aligned. }
          DebugSection^.SizeOfRawData := ALIGN(DebugStream.Size, LFileAlignment);
          { Fix SizeOfImage = VirtualSize of image =  PreviousSection.VirtualAddress + PreviousSection.VirtualSize. }
          pSizeOfImage^ := VA + DebugSection^.Misc.VirtualSize;

          { Copy debug data from DebugStream to debug section. }
          P := ImageRvaToVa(NtHeaders, MapAddress, VA, nil);
          DebugStream.Read(P^, DebugStream.Size);

          with DebugDirectoryEntry^ do
          begin
            Size := InternalDebugData.NumberOfDebugDirectories;
            VirtualAddress := VA;
            DebugDirectory := P;
            for I := 1 to Size do
            begin
              DebugDirectory^.PointerToRawData := (DebugDirectory^.PointerToRawData - InternalDebugData.PointerToRawData) + DebugSection^.PointerToRawData;
              DebugDirectory^.AddressOfRawData := (DebugDirectory^.AddressOfRawData - InternalDebugData.DebugVirtualAddress) + VA;
              Inc(DebugDirectory);
            end;
          end;

          { Calculate end of file }
          FP := DebugSection^.PointerToRawData + DebugSection^.SizeOfRawData;
        end;

        UnmapViewOfFile(MapAddress);
      end;
      CloseHandle(MapObj);
    end;
    if FP <> 0 then
    begin
      Result := True;
      SetFilePointer(FileHandle, FP, nil, FILE_BEGIN);
      SetEndOfFile(FileHandle);
    end;
    CloseHandle(FileHandle);
  end;
end;

end.
