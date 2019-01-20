// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.DebugInfo
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.DebugInfo.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

// ===================> CHANGE LOG <===================
// [4/28/17]:
// - Changed MAX_SEGMENT_UNITS from (1000) to (10000).

unit DebugEngine.DebugInfo;

interface

{$I DebugEngine.inc}
{ .$DEFINE DEVMODE }

uses
  WinApi.Windows,
  WinApi.ImageHlp,
  WinApi.PsApi,
  System.SysUtils,
  System.Classes,
  System.ZLib,
  System.RegularExpressions;

const
  DelphiMapFileExtension = '.map';
  SMapFileExtension = '.smap';
  SMapResType = 'SMAP';
  SMapResName = 'Map';
  SMapSignature = $50414D53; { SMAP }
  SMapVersion = $01;

  DelphiDebugSection = '.debug';

  SDebugSection = '.SDEBUG';

  SEGMENT_NAME_LENGTH = 8;

{$REGION 'MapTypes'}

type
  PSMapChar = PAnsiChar;
  SMapChar = AnsiChar;

  TMapNotification = (mnNone, mnSegments, mnUnits, mnPublicsByName, mnPublicsByValue, mnLineLocations);

  { SMap options }
  TSMapOptions = set of (moCompress);
  {
    moCompress => Compress (ZIP) smap data.
  }

  { TMapLocation }
  TMapLocation = (mlNone, mlSection, mlResource, mlDisk);

  {
    - mlNone = There is no map associated with module.
    - mlSection = Map was inserted into SDEBUG section.
    - mlResource = Map was inserted into PE resource.
    - mlDisk = Remote map (Delphi map) found next the PE.

  }

  { TSMapHeader }
  TSMapHeader = packed record
    Signature: Cardinal; // SMAP signature.
    Version: Byte; // SMAP version.
    Flags: Cardinal; // SMAP options/flags.
    Size: Cardinal; // Size of smap file without counting align block.
    cSize: Cardinal; // Size of compressed smap file.
    NumberOfSegments: Byte; // Number of segments.
    NumberOfUnits: Word; // Number of units.
    NumberOfSymbols: DWORD; // Number of Symbols.
    NumberOfSourceLocations: Word; // Number of source locations.
    OffsetToUnits: Cardinal; // Offset from header to first unit struct (TSMapUnit).
    OffsetToSymbols: Cardinal; // Offset from header to first symbol struct (TSMapSymbol).
    OffsetToSourceLocations: Cardinal; // Offset from header to first source location struct (TSMapSourceLocation).
  end;

  PSMapHeader = ^TSMapHeader;

  TSMapSegment = packed record
    SegId: Byte; // Segment id.
    SegStartAddress: Cardinal; // Segment start offset.
    SegLength: Cardinal; // Segment SegLength.
    SegName: array [0 .. SEGMENT_NAME_LENGTH - 1] of SMapChar; // Segment name.
  end;

  PSMapSegment = ^TSMapSegment;

  TSMapUnit = packed record
    UnitSegId: Byte; // Segment id.
    UnitOffset: Cardinal; // Unit offset start address.
    UnitLength: Cardinal; // Unit SegLength.
    UnitNameLength: Word; // Unit name SegLength.
    { Unit name follows TSMapUnit struct }
    { Next unit = ThisUnit + (SizeOf(TSMapUnit) + UnitNameLength). }
    UnitName: array [0 .. 0] of SMapChar; // Unit name.
  end;

  PSMapUnit = ^TSMapUnit;

  TSMapSymbol = packed record
    SymbolSegId: Byte;
    SymbolOffset: Cardinal;
    SymbolNameLength: Word;
    SymbolName: array [0 .. 0] of SMapChar;
  end;

  PSMapSymbol = ^TSMapSymbol;

  TSMapSourceLocation = packed record
    SegId: Byte;
    NumberOfLineNumbers: Word;
    SourceLocationLength: Word;
    SourceLocation: array [0 .. 0] of SMapChar;
  end;

  PSMapSourceLocation = ^TSMapSourceLocation;

  TSMapLineNumber = packed record
    Offset: Cardinal;
    LineNumber: Cardinal;
  end;

  PSMapLineNumber = ^TSMapLineNumber;

  TRuntimeSegment = record
    SegId: Cardinal;
    SegStartAddress: Pointer;
    SegEndAddress: Pointer;
    SegLength: Cardinal;
  end;

  PRuntimeSegment = ^TRuntimeSegment;

  TLineNumberSource = record
    Line: PSMapLineNumber;
    Source: PSMapSourceLocation;
  end;

  PLineNumberSource = ^TLineNumberSource;

{$ENDREGION 'MapTypes'}
  TModule = class;
  TDebugInfoBase = class;
  TDebugInfoMapBase = class;

  TAddressInfo = record
    SymbolAddress: Pointer;
    LineNumber: Cardinal;
    SymbolName: string;
    UnitName: string;
    SourceLocation: string;
    DebugSource: TDebugInfoBase;
    SymbolIndex: Integer; // Export Index.
  end;

  PAddressInfo = ^TAddressInfo;

  TAddressInfoMask = (
    { aimNone = No mask will be applied.
      GetAddressInfo function will query all info :
      - SymbolAddress.
      - SymbolName.
      - UnitName.
      - DebugSource.
      - LineNumber.
      - SourceLocation.
    }
    aimNone,

    { aimAddress = GetAddressInfo function will query only :
      - SymbolAddress.
      - DebugSource.
    }
    aimAddress,

    { aimSymbolName =  GetAddressInfo function will query only :
      - SymbolAddress.
      - SymbolName.
      - UnitName.
      - DebugSource.
    }
    aimSymbolName);

{$REGION 'DebugClass'}

  TModule = class(TObject)
  private
    FModule: THandle;
    FStartAddress: Pointer;
    FEndAddress: Pointer;
    FSize: Cardinal;
    FBaseName: string;
    FFileName: string;
    FBorlandModule: Boolean;
    FMapLocation: TMapLocation;
    FDebugInfo: TDebugInfoBase;
    function GetMapLocation: TMapLocation;
    function GetImageBase: NativeUInt;
    function GetIsBorlandModule: Boolean;
  protected
    procedure CreateDebugInfo;
    procedure InitializeModule;
  public
    constructor Create(ModuleHandle: THandle); virtual;
    destructor Destroy; override;
    function IsAddressInModuleRange(Address: Pointer): Boolean;
    property ModuleHandle: THandle read FModule;
    property SegStartAddress: Pointer read FStartAddress;
    property SegEndAddress: Pointer read FEndAddress;
    property Size: Cardinal read FSize;
    property BaseName: string read FBaseName;
    property FileName: string read FFileName;
    property MapLocation: TMapLocation read FMapLocation;
    property ImageBase: NativeUInt read GetImageBase;
    property IsBorlandModule: Boolean read FBorlandModule;
    property DebugInfo: TDebugInfoBase read FDebugInfo;
  end;

  TDebugInfoBase = class(TObject)
  private
    FModule: TModule;
  public
    /// <summary>
    /// All descendants class must implement this function.
    /// </summary>
    function GetAddressInfo(Address: Pointer; out Info: TAddressInfo; Mask: TAddressInfoMask): Boolean; virtual; abstract;
    function GetSymbolAddress(const UnitName, SymbolName: string): Pointer; virtual; abstract;
    function GetAddressFromIndex(Index: Integer): Pointer; virtual; abstract;
    constructor Create(Module: TModule); virtual;
    destructor Destroy; override;
    property Module: TModule read FModule;
  end;

  TDebugInfoExport = class(TDebugInfoBase)
  private type
    TExportInfo = record
      Address: Pointer;
      Ord: Word;
      Hint: Word;
      Name: string;
    end;

    PExportInfo = ^TExportInfo;
  private
    FExportList: TList;
  protected
    procedure CreateExportList;
  public
    function GetSymbolAddress(const UnitName, SymbolName: string): Pointer; override;
    function GetAddressInfo(Address: Pointer; out Info: TAddressInfo; Mask: TAddressInfoMask): Boolean; override;
    function GetAddressFromIndex(Index: Integer): Pointer; override;
    constructor Create(Module: TModule); override;
    destructor Destroy; override;
  end;

  TDebugInfoMapBase = class(TDebugInfoBase)
  private
    FMapStream: TMemoryStream;
  protected
    function ProcessMap: Boolean; virtual; abstract;
  public
    function LoadFromStream(Stream: TStream): Boolean;
    function LoadFromFile(const MapFileName: string): Boolean;
    constructor Create(Module: TModule); override;
    destructor Destroy; override;
    property MapStream: TMemoryStream read FMapStream;
  end;

  TDebugInfoSMap = class(TDebugInfoMapBase)
  private
    FRTSegments: TList;
    FUnits: TList;
    FSymbols: TList;
    FLineSources: TList;
    function GetRTSegmentFromSegIndex(SegIndex: Integer): PRuntimeSegment;
    function GetRTSegmentFromAddress(Address: Pointer): PRuntimeSegment;
    function GetUnit(Address: Pointer; PRtSeg: PRuntimeSegment): PSMapUnit;
    function GetLineNumberSource(Address: Pointer; PRtSeg: PRuntimeSegment): PLineNumberSource;
  protected
    function UnZip: Boolean;
    function ProcessMap: Boolean; override;
  public
    function GetAddressInfo(Address: Pointer; out Info: TAddressInfo; Mask: TAddressInfoMask): Boolean; override;
    function GetSymbolAddress(const UnitName, SymbolName: string): Pointer; override;
    function GetAddressFromIndex(Index: Integer): Pointer; override;
    constructor Create(Module: TModule); override;
    destructor Destroy; override;
  end;

  TModules = class(TObject)
  private
    FModulesList: TList;
    function GetModuleFromAddress(Address: Pointer): TModule;
    function GetModuleFromModuleHandle(ModuleHandle: THandle; RegisterNoExists: Boolean): TModule;
  protected
    function AddModule(ModuleHandle: THandle): TModule;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ModuleFromAddress[Address: Pointer]: TModule read GetModuleFromAddress;
  end;

  TCustomTxtMapParser = class(TObject)
  private
    FLines: TStringList;
  protected
    /// <summary> Get notified when <see cref="Parse"/> function is about to process a new region.
    /// </summary>
    /// <returns>
    /// Return <c>True</c> to keep processing actual region.
    /// <para></para>
    /// Return <c>False</c> to exclude actual region.
    /// </returns>
    /// <remarks> This function must be implemented in all descendants class.
    /// </remarks>
    function Notify(Notification: TMapNotification): Boolean; virtual; abstract;
    procedure ProcessSegment(SegId: Integer; SegOffset: Cardinal; SegLength: Cardinal; const SegName, SegClass: string); virtual; abstract;
    procedure ProcessUnit(SegId: Integer; UnitOffset: Cardinal; UnitLength: Cardinal; const SegClass, SegName, Group, UnitName: string; ACBP, ALIGN: Integer); virtual; abstract;
    procedure ProcessPublicsByName(SegId: Integer; SymbolOffset: Cardinal; const SymbolName: string); virtual; abstract;
    procedure ProcessPublicsByValue(SegId: Integer; SymbolOffset: Cardinal; const SymbolName: string); virtual; abstract;
    procedure ProcessLocation(const UnitName, Source, SegmentName: string); virtual; abstract;
    procedure ProcessLine(LineNumber, SegId, Offset: Integer); virtual; abstract;
  public
    /// <summary> Start parsing map file.
    /// </summary>
    function Parse: Boolean;
    constructor Create(const MapFileName: string); virtual;
    destructor Destroy; override;
  end;

{$ENDREGION 'DebugClass'}

  // ---------------------------------------------------------------------------------------------------------------------------
{$REGION 'PublicFunctions'}

  /// <summary>  Convert Delphi map file to SMAP file format.
  /// </summary>
  /// <param name="MapFile"> Delphi map file name.
  /// </param>
  /// <param name="Options"> See
  /// <see cref="TSMapOptions" />
  /// </param>
  /// <returns> If the function succeeds, the return value is the converted map file (smap) size.
  /// <para></para>
  /// If the function fails, the return value is zero.
  /// </returns>
  /// <remarks> The converted map (SMAP) file will exist in the same MapFile folder.
  /// </remarks>
function ConvertMapToSMap(const MapFile: string; const Options: TSMapOptions = [moCompress]): Integer; overload;

/// <summary>  Convert Delphi map data to SMAP data format.
/// </summary>
/// <param name="SrcPtr"> Pointer to a buffer that holds Delphi map data.
/// </param>
/// <param name="DstPtr"> Pointer to a buffer that will hold converted map.
/// </param>
/// <param name="Options"> See
/// <see cref="TSMapOptions" />
/// </param>
/// <returns> If the function succeeds, the return value is the converted map data (smap) size.
/// <para></para>
/// If the function fails, the return value is zero.
/// </returns>
/// <remarks> It's true that the smap size will be less than the original map size.
/// However, it's recommended to set DstPtr size equal to the SrcPtr size.
/// </remarks>
function ConvertMapToSMap(const SrcPtr, DstPtr: Pointer; Options: TSMapOptions): Integer; overload;

/// <summary> Convert Delphi map data to SMAP data format.
/// </summary>
/// <param name="SrcStream"> Memory stream that holds Delphi map data.
/// </param>
/// <param name="DstStream"> Memory stream that will hold converted map data.
/// </param>
/// <param name="Options"> See
/// <see cref="TSMapOptions" />
/// </param>
/// <returns> If the function succeeds, the return value is the converted map data (smap) size.
/// <para></para>
/// If the function fails, the return value is zero.
/// </returns>
/// <remarks> The function sets DstStream size to the result.
/// </remarks>
function ConvertMapToSMap(SrcStream, DstStream: TMemoryStream; Options: TSMapOptions): Integer; overload;

/// <summary> Retrieve address info.
/// </summary>
/// <param name="Address"> Address to obtain information on.
/// </param>
/// <param name= "Info"> Info record output for the specified address.
/// </param>
/// <param name= "Mask"> Query only specified info.
/// This is very useful when processing too many address.
/// Use this mask to boost function speed.
/// For more information see <see cref="TAddressInfoMask"/>.
/// </param>
/// <returns> If the function succeeds, the return value is True.
/// </returns>
function GetAddressInfo(Address: Pointer; out Info: TAddressInfo; const Mask: TAddressInfoMask = aimNone): Boolean;

/// <summary> Retrieve address of symbol from symbol name.
/// </summary>
/// <param name="ModuleHandle"> Module handle where the symbol is located.
/// </param>
/// <param name="UnitName"> Unit name where the symbol was declared.
/// </param>
/// <param name="SymbolName"> Symbol name.
/// </param>
/// <returns> If the function succeeds, the return value is the address of the symbol. Otherwise it returns nil.
/// </returns>
/// <remarks>
/// <para> If <c>ModuleHandle</c> was not specified (0), the function will use the current module handle.
/// </para>
/// <para> <c>UnitName</c> parameter is optional. It's useful when the symbol is declared in more than unit.
/// </para>
/// </remarks>
function GetSymbolAddress(ModuleHandle: THandle; const UnitName, SymbolName: string): Pointer;

/// <summary> Retrieve next symbol address from the current symbol address.
/// </summary>
/// <param name="Address"> Address of the current symbol.
/// </param>
/// <returns> If the function succeeds, the return value is the next address of the current symbol. Otherwise it returns nil.
/// </returns>
function GetNextSymbolAddress(Address: Pointer): Pointer;

/// <summary> Retrieve the size of a function in bytes.
/// </summary>
/// <param name="Address"> Address of the function.
/// </param>
/// <returns> If the function succeeds, the return value is the size of function (opcodes size). Otherwise it returns 0.
/// </returns>
function GetSizeOfFunction(Address: Pointer): Integer;

{$ENDREGION 'PublicFunctions'}
// ------------------------------------------------------------------------------------
{$REGION 'MiscFunctions'}
{ Basically those functions were not designed for public use,
  However, I think that it is better to keep them public. }

function GetModuleBaseName(Module: HMODULE): string;
function GetModuleFileName(Module: HMODULE): string;
function GetModuleHandleFromAddress(Address: Pointer): THandle;
function MapLocationToStr(Location: TMapLocation): string;

{$ENDREGION 'MiscFunctions'}

implementation

uses
  DebugEngine.PeUtils;

{$B-}   // Force B-!
{$REGION 'InternalDebugUtils'}
{$IFDEF DEVMODE}

procedure Bp;
asm
  int 3
end;

{$ENDIF DEVMODE}
{$ENDREGION 'InternalDebugUtils'}
{$REGION 'GLOBAL'}
const
  PAGE_EXECUTE_MASK = PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY;
  { Regular expressions patterns used by TCustomTxtMapParser }

  HintSegmentRegExPattern = '^\s*Start\s+Length\s+Name\s+Class\s*$';
  HintUnitRegExPattern = '^\s*Detailed\smap\sof\ssegments\s*$';
  HintPublicsByNameRegExPattern = '^\s*Address\s+Publics\s+by\s+Name\s*$';
  HintPublicsByValueRegExPattern = '^\s*Address\s+Publics\s+by\s+Value\s*$';

  SegmentRegExPattern = '^\s(\d{4}):([0-9A-F]{8})\s([0-9A-F]{8})H\s(\.\w+)\s+(\w+)$';
  UnitRegExPattern = '^\s(\d{4}):([0-9A-F]{8})\s([0-9A-F]{8})\sC=(\w+)\s+S=(\.\w+)\s+G=\(*(\w+)\)*\s+M=(\S+)(?:\s+(ALIGN|ACBP)=(\w+))*\s*$';
  SymbolRegExPattern = '^\s(\d{4}):([0-9A-F]{8})\s+(.+)$';
  LocationRegExPattern = '^\s*^Line\snumbers\sfor\s(.+?)\((.+?)\)\ssegment\s+(\.\w+)\s*$';
  LineRegExPattern = '\s+(\d+)\s(\d{4}):([0-9A-F]{8})';

  { Place all global variables here ! }
var

  GlobalModules: TModules = nil;

  GlobalLock: TObject = nil;
  RegxLock: TObject = nil;
  RegularExpressionsCompiled: Boolean = False;

  { Hint Regular expressions for TCustomTxtMapParser parser. }
  HintSegmentRegEx: TRegEx;
  HintUnitRegEx: TRegEx;
  HintPublicsByNameRegEx: TRegEx;
  HintPublicsByValueRegEx: TRegEx;

  { Regular expressions for TCustomTxtMapParser parser. }
  SegmentRegEx: TRegEx;
  UnitRegEx: TRegEx;
  SymbolRegEx: TRegEx;
  LocationRegEx: TRegEx;
  LineRegEx: TRegEx;

  { Put all functions that initialize global variables here ! }

procedure CompileRegularExpressions;
begin
  if RegularExpressionsCompiled then
    Exit;

  HintSegmentRegEx := TRegEx.Create(HintSegmentRegExPattern, [roCompiled, roSingleLine, roIgnoreCase]);
  HintUnitRegEx := TRegEx.Create(HintUnitRegExPattern, [roCompiled, roSingleLine, roIgnoreCase]);
  HintPublicsByNameRegEx := TRegEx.Create(HintPublicsByNameRegExPattern, [roCompiled, roSingleLine, roIgnoreCase]);
  HintPublicsByValueRegEx := TRegEx.Create(HintPublicsByValueRegExPattern, [roCompiled, roSingleLine, roIgnoreCase]);

  SegmentRegEx := TRegEx.Create(SegmentRegExPattern, [roCompiled, roSingleLine]);
  UnitRegEx := TRegEx.Create(UnitRegExPattern, [roCompiled, roSingleLine]);
  SymbolRegEx := TRegEx.Create(SymbolRegExPattern, [roCompiled, roSingleLine]);
  LocationRegEx := TRegEx.Create(LocationRegExPattern, [roCompiled, roSingleLine]);
  LineRegEx := TRegEx.Create(LineRegExPattern, [roCompiled, roSingleLine]);

  RegularExpressionsCompiled := True;
end;

procedure NeedGlobalModules;
begin
  if Assigned(GlobalModules) then
    Exit;
  TMonitor.Enter(GlobalLock);
  try
    if not Assigned(GlobalModules) then
      GlobalModules := TModules.Create;
  finally
    TMonitor.Exit(GlobalLock);
  end;
end;

procedure NeedCompiledRegularExpressions;
begin
  if RegularExpressionsCompiled then
    Exit; // No need to enter the lock.
  TMonitor.Enter(RegxLock);
  try
    CompileRegularExpressions;
  finally
    TMonitor.Exit(RegxLock);
  end;
end;

{$ENDREGION 'GLOBAL'}

function GetAddressInfo(Address: Pointer; out Info: TAddressInfo; const Mask: TAddressInfoMask = aimNone): Boolean;
var
  LModule: TModule;
begin
  NeedGlobalModules;
  try
    LModule := GlobalModules.ModuleFromAddress[Address];
    if Assigned(LModule) and Assigned(LModule.DebugInfo) then
    begin
      Result := LModule.DebugInfo.GetAddressInfo(Address, Info, Mask);
      Exit;
    end;
  except
    // Do nothing.
  end;
  Result := False;
end;

function GetSymbolAddress(ModuleHandle: THandle; const UnitName, SymbolName: string): Pointer;
var
  Module: TModule;
begin
  Result := nil;
  if ModuleHandle = 0 then
    ModuleHandle := GetModuleHandle(nil);

  NeedGlobalModules;
  Module := GlobalModules.GetModuleFromModuleHandle(ModuleHandle, True);
  if Assigned(Module) then
    Result := Module.DebugInfo.GetSymbolAddress(UnitName, SymbolName);
end;

function GetNextSymbolAddress(Address: Pointer): Pointer;
var
  Info: TAddressInfo;
begin
  if GetAddressInfo(Address, Info, aimAddress) and (Assigned(Info.DebugSource)) then
  begin
    Result := Info.DebugSource.GetAddressFromIndex(Info.SymbolIndex + 1);
    Exit;
  end;
  Result := nil;
end;

function GetSizeOfFunction(Address: Pointer): Integer;
var
  mbi: TMemoryBasicInformation;
  NextAddress: Pointer;
begin
  { SizeOfFunction = NextAdjacentAddress - StartAddress of the function. }
  if (VirtualQuery(Address, mbi, SizeOf(TMemoryBasicInformation)) > 0) and (mbi.Protect and PAGE_EXECUTE_MASK <> $00) then
  begin
    NextAddress := GetNextSymbolAddress(Address);
    if Assigned(NextAddress) then
    begin
      Result := NativeUInt(NextAddress) - NativeUInt(Address);
      Exit;
    end;
  end;
  Result := 0;
end;

{$REGION 'Misc'}

function MapLocationToStr(Location: TMapLocation): string;
begin
  Result := EmptyStr;
  case Location of
    mlSection: Result := 'Section';
    mlResource: Result := 'Resource';
    mlDisk: Result := 'Disk'
  else Result := 'none';
  end;
end;

function ALIGN(Value, Alignment: Cardinal): Cardinal;
begin
  Result := Value;
  if Value mod Alignment = 0 then
    Exit;
  Result := (Result + Alignment - 1) div Alignment * Alignment;
end;

function IsAddressInRange(Address: Pointer; const SegStartAddress, SegEndAddress: Pointer): Boolean;
begin
  Result := (NativeUInt(Address) >= NativeUInt(SegStartAddress)) and (NativeUInt(Address) < NativeUInt(SegEndAddress));
end;

function GetModuleFileName(Module: HMODULE): string;
var
  Buffer: array of Char;
  nSize: Cardinal;
label DoItAgain;
begin
  nSize := MAX_PATH;
DoItAgain: SetLength(Buffer, nSize);
  if WinApi.Windows.GetModuleFileName(Module, @Buffer[0], nSize) = 0 then
    Exit(EmptyStr);
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then
  begin
    { Insufficient buffer length => Realloc memory and try again. }
    Inc(nSize, nSize);
    goto DoItAgain;
  end;
  Result := String(PChar(@Buffer[0]));
end;

function GetModuleBaseName(Module: HMODULE): string;
var
  Buffer: array [0 .. MAX_PATH] of Char;
  nSize: Cardinal;
begin
  Result := EmptyStr;
  nSize := MAX_PATH - 1;
  if WinApi.PsApi.GetModuleBaseName(GetCurrentProcess, Module, @Buffer[0], nSize) > 0 then
    Result := String(PChar(@Buffer[0]));
end;

function GetModuleHandleFromAddress(Address: Pointer): THandle;
var
  mbi: TMemoryBasicInformation;
begin
  if (VirtualQuery(Address, mbi, SizeOf(TMemoryBasicInformation)) > 0) and (mbi.State = MEM_COMMIT) then
    Exit(HMODULE(mbi.AllocationBase));
  Result := 0;
end;

function SMapOptionsToRaw(const Options): DWORD;
begin
  { Convert SMapOptions type to a raw dword value. }

{$IF SizeOf(TSMapOptions) = 1}
  Result := PByte(@Options)^;
{$ELSEIF SizeOf(TSMapOptions) = 2}
  Result := PWord(@Options)^;
{$ELSEIF SizeOf(TSMapOptions) = 4}
  Result := PDWORD(@Options)^;
{$ELSE}
{$MESSAGE Fatal 'Error size of TSMapOptions > 4 bytes.'}
{$ENDIF}
end;

function RawToSMapOptions(Options: DWORD): TSMapOptions;
begin
  { Convert a raw dword type to SMapOptions. }

{$IF SizeOf(TSMapOptions) = 1}
  PByte(@Result)^ := Byte(Options);
{$ELSEIF SizeOf(TSMapOptions) = 2}
  PWord(@Result)^ := Word(Options);
{$ELSEIF SizeOf(TSMapOptions) = 4}
  PDWORD(@Result)^ := Options;
{$ELSE}
{$MESSAGE Fatal 'Error size of TSMapOptions > 4 bytes.'}
{$ENDIF}
end;

{$ENDREGION 'Misc'}
{$REGION 'MapParser'}
{ TCustomTxtMapParser }

constructor TCustomTxtMapParser.Create(const MapFileName: string);
begin
  FLines := TStringList.Create;
  if FileExists(MapFileName) then
    FLines.LoadFromFile(MapFileName);
end;

destructor TCustomTxtMapParser.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TCustomTxtMapParser.Parse: Boolean;
var
  I: Integer;
  S: string;
  Match: TMatch;
  Notification: TMapNotification;
  ALIGN: Integer;
  ACBP: Integer;
  Matches: TMatchCollection;
  KeepProcess: Boolean;
label ProcessSegmentsLabel;
label ProcessUnitsLabel;
label ProcessPublicsByNameLabel;
label ProcessPublicsByValueLabel;
label ProcessLineLocationsLabel;

begin
  Result := FLines.Count > 0;
  if not Result then
    Exit;

  NeedCompiledRegularExpressions;

  Notification := mnNone;
  KeepProcess := False;
  for I := 0 to FLines.Count - 1 do
  begin
    S := FLines[I];
    if S.Trim.IsEmpty then
      Continue; // KeepProcess Next valid line.

    case Notification of
      mnSegments: goto ProcessSegmentsLabel;
      mnUnits: goto ProcessUnitsLabel;
      mnPublicsByName: goto ProcessPublicsByNameLabel;
      mnPublicsByValue: goto ProcessPublicsByValueLabel;
      mnLineLocations: goto ProcessLineLocationsLabel;
    end;

    Match := HintSegmentRegEx.Match(S);
    if Match.Success then
    begin
      KeepProcess := Notify(mnSegments);
      Notification := mnSegments;
    end;
    Continue;
  ProcessSegmentsLabel: // Process Segments.
    if KeepProcess then
    begin
      Match := SegmentRegEx.Match(S);
      if Match.Success then
      begin
        with Match do
          ProcessSegment(StrToInt(Groups[1].Value), StrToInt('$' + Groups[2].Value), StrToInt('$' + Groups[3].Value), Groups[4].Value, Groups[5].Value);
        Continue;
      end;
    end;
    Match := HintUnitRegEx.Match(S);
    if Match.Success then
    begin
      KeepProcess := Notify(mnUnits);
      Notification := mnUnits;
    end;
    Continue;

  ProcessUnitsLabel: // Process Units.
    if KeepProcess then
    begin
      Match := UnitRegEx.Match(S);
      if Match.Success then
      begin
        ALIGN := 0;
        ACBP := 0;
        with Match do
        begin
          if Groups.Count > 8 then
          begin
            if SameText(Groups[8].Value, 'ALIGN') then
              ALIGN := StrToInt('$' + Groups[9].Value)
            else if SameText(Groups[8].Value, 'ACBP') then
              ACBP := StrToInt('$' + Groups[9].Value);
          end;
          ProcessUnit(StrToInt(Groups[1].Value), StrToInt('$' + Groups[2].Value), StrToInt('$' + Groups[3].Value), Groups[4].Value, Groups[5].Value, Groups[6].Value, Groups[7].Value, ACBP, ALIGN);
        end;
        Continue;
      end;
    end;
    Match := HintPublicsByNameRegEx.Match(S);
    if Match.Success then
    begin
      KeepProcess := Notify(mnPublicsByName);
      Notification := mnPublicsByName;
    end;
    Continue;

  ProcessPublicsByNameLabel: // Process publics by name.
    if KeepProcess then
    begin
      Match := SymbolRegEx.Match(S);
      if Match.Success then
      begin
        with Match do
          ProcessPublicsByName(StrToInt(Groups[1].Value), StrToInt('$' + Groups[2].Value), Groups[3].Value);
        Continue;
      end;
    end;
    Match := HintPublicsByValueRegEx.Match(S);
    if Match.Success then
    begin
      KeepProcess := Notify(mnPublicsByValue);
      Notification := mnPublicsByValue;
    end;
    Continue;

  ProcessPublicsByValueLabel: // Process publics by value.
    if KeepProcess then
    begin
      Match := SymbolRegEx.Match(S);
      if Match.Success then
      begin
        with Match do
          ProcessPublicsByValue(StrToInt(Groups[1].Value), StrToInt('$' + Groups[2].Value), Groups[3].Value);
        Continue;
      end;
    end;

  ProcessLineLocationsLabel: // Process line locations.
    if (Notification = mnLineLocations) and not KeepProcess then
      Continue;
    Match := LocationRegEx.Match(S);
    if Match.Success then
    begin
      if Notification <> mnLineLocations then
      begin
        KeepProcess := Notify(mnLineLocations);
        Notification := mnLineLocations;
      end;
      if not KeepProcess then
        Continue;
      with Match do
        ProcessLocation(Groups[1].Value, Groups[2].Value, Groups[3].Value);
      Continue;
    end;
    Matches := LineRegEx.Matches(S);
    if Matches.Count > 0 then
    begin
      for Match in Matches do
      begin
        with Match do
          ProcessLine(StrToInt(Groups[1].Value), StrToInt(Groups[2].Value), StrToInt('$' + Groups[3].Value));
      end;
    end;
  end;

end;
{$ENDREGION 'MapParser'}
{$REGION 'MapConverter'}

const
  HintArrayLength = 4;
  HintArrayCharsLength = 8;
  MAX_SEGMENTS = 31;
  MAX_SEGMENT_UNITS = 10000;

type
  THintArray = array [0 .. HintArrayLength - 1] of array [0 .. HintArrayCharsLength - 1] of SMapChar;

  { ==> First item is used as a cashe <==
    ==> Last item is used as segment's units count and not PSMapUnit !!! <== }
  TSegmentsUnits = array [0 .. MAX_SEGMENTS - 1] of array [0 .. MAX_SEGMENT_UNITS] of PSMapUnit;
  PSegmentsUnits = ^TSegmentsUnits;

const
  CharToHexArray: array [SMapChar] of ShortInt = ( //
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, //
    $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, //
    -1, -1, -1, -1, -1, -1, -1, //
    $0A, $0B, $0C, $0D, $0E, $0F, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, $0A, $0B, $0C, $0D, $0E, $0F, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1);

  HintSegArray: THintArray = ('Start', 'Length', 'Name', 'Class');
  HintUnitArray: THintArray = ('Detailed', 'map', 'of', 'segments');
  HintPublicsByValueArray: THintArray = ('Address', 'Publics', 'by', 'Value');
  HintLineNumbersArray: THintArray = ('Line', 'numbers', 'for', '');

function ConvertMapToSMap(const SrcPtr, DstPtr: Pointer; Options: TSMapOptions): Integer;
var
  LCurrPos: PSMapChar;
  PSegsUnits: PSegmentsUnits;
{$REGION 'LocalFunctions'}
  procedure NextBeginOfLine;
  begin
    while not(LCurrPos^ in [#00, #10, #13]) do
      Inc(LCurrPos);
    while LCurrPos^ in [#00, #10, #13, ' '] do
      Inc(LCurrPos);
  end;

  function ReadDecValue: Integer;
  begin
    Result := 0;
    while PByte(LCurrPos)^ in [$30 .. $39] do
    begin
      Result := (Result * 10) + (PByte(LCurrPos)^ - $30);
      Inc(LCurrPos);
    end;
  end;

  function ReadHexValue: Cardinal;
  var
    L: ShortInt;
  begin
    Result := 0;
    while True do
    begin
      L := CharToHexArray[LCurrPos^];
      if L = -1 then
        Break;
      Result := (Result shl $04) or Byte(L);
      Inc(LCurrPos);
    end;
  end;

  function ReachedHint(const HintArray: THintArray): Boolean;
  var
    I: Integer;
    J: Integer;
    C: SMapChar;
    P: PSMapChar;
  begin
    P := LCurrPos;
    for I := 0 to HintArrayLength - 1 do
    begin
      while P^ = ' ' do
        Inc(P);
      for J := 0 to HintArrayCharsLength - 1 do
      begin
        C := HintArray[I][J];
        if (C = #00) then
          Break;
        if (C <> P^) then
          Exit(False);
        Inc(P);
      end;
    end;
    Result := True;
  end;

  function GetUnit(Seg: Integer; Offset: Cardinal): PSMapUnit;
  var
    n: Integer;
    I: Integer;
  begin
    n := Integer(PSegsUnits^[Seg][MAX_SEGMENT_UNITS]);
    Assert(n and $FFFF0000 = $00000000);
    for I := 0 to n do
    begin
      Result := PSegsUnits^[Seg][I];
      if not Assigned(Result) then
        Continue;
      with Result^ do
        if (Offset >= UnitOffset) and (Offset < UnitOffset + UnitLength) then
        begin
          PSegsUnits^[Seg][0] := Result; // Cashe this unit.
          Exit;
        end;
    end;
    Result := nil;
  end;

{$ENDREGION 'LocalFunctions'}

{ TODO: exit if ReachedHint returns False. }
var
  PHeader: PSMapHeader;
  PTmpHeader: PSMapHeader;
  PSegment: PSMapSegment;
  PUnit: PSMapUnit;
  PSymbol: PSMapSymbol;
  PSrc: PSMapSourceLocation;
  PLine: PSMapLineNumber;
  L: Integer;
  Buffer: Pointer;
begin
  Result := 0;

  LCurrPos := SrcPtr;
  PHeader := DstPtr;
  PSegsUnits := AllocMem(SizeOf(TSegmentsUnits));

  with PHeader^ do
  begin
    Signature := SMapSignature;
    Version := SMapVersion;
    Flags := SMapOptionsToRaw(Options);
  end;
  PSegment := PSMapSegment(PByte(PHeader) + SizeOf(TSMapHeader));
  // First segment.

  { Initialize datas and make the compiler happy :) }
  PUnit := nil;
  // PSymbol := nil;
  PLine := nil;

  NextBeginOfLine;
  if ReachedHint(HintSegArray) then
  begin
    NextBeginOfLine;
    repeat
      Inc(PHeader^.NumberOfSegments);
      with PSegment^ do
      begin
        SegId := ReadDecValue;
        Inc(LCurrPos); // Skip ":".
        SegStartAddress := ReadHexValue;
        while (LCurrPos^ = ' ') do
          Inc(LCurrPos);
        SegLength := ReadHexValue;
        while (LCurrPos^ <> '.') do
          // Go to segment name.
          Inc(LCurrPos);
        L := 0;
        while (LCurrPos^ <> ' ') do
        begin
          SegName[L] := LCurrPos^;
          Inc(L);
          Inc(LCurrPos);
        end;
      end;
      Inc(PSegment); // Next segment.
      NextBeginOfLine;
    until LCurrPos^ <> '0';
  end;
  if ReachedHint(HintUnitArray) then
  begin
    NextBeginOfLine;
    PUnit := PSMapUnit(PSegment);
    PHeader^.OffsetToUnits := Cardinal(NativeUInt(PUnit) - NativeUInt(PHeader));

    while LCurrPos^ = '0' do
    begin
      Inc(PHeader^.NumberOfUnits);
      with PUnit^ do
      begin
        UnitSegId := ReadDecValue;
        L := Integer(PSegsUnits^[UnitSegId][MAX_SEGMENT_UNITS]);
        PSegsUnits^[UnitSegId][MAX_SEGMENT_UNITS] := Pointer(L + 1);
        PSegsUnits^[UnitSegId][L] := PUnit;
        Inc(LCurrPos); // Skip ":".
        UnitOffset := ReadHexValue;
        while LCurrPos^ = ' ' do
          Inc(LCurrPos);
        UnitLength := ReadHexValue;
        { Go to unit name. }
        while not((LCurrPos^ = ' ') and ((LCurrPos + 1)^ = 'M') and ((LCurrPos + 2)^ = '=')) do
          Inc(LCurrPos);
        Inc(LCurrPos, 3); // Skip " M=".
        L := 0;
        while not((LCurrPos + L)^ in [' ', #13, #10]) do
          Inc(L); // Unit name SegLength.
        Move(LCurrPos^, Pointer(@UnitName[0])^, L);
        UnitNameLength := L;
      end;
      NextBeginOfLine;
      Inc(PUnit);
      Inc(PByte(PUnit), L);
    end;
  end;

  repeat
    NextBeginOfLine;
    { Limit ReachedHint call ! }
    while LCurrPos^ <> 'A' do
      NextBeginOfLine;
  until ReachedHint(HintPublicsByValueArray);

  NextBeginOfLine;
  PSymbol := PSMapSymbol(PUnit);
  PHeader^.OffsetToSymbols := Cardinal(NativeUInt(PSymbol) - NativeUInt(PHeader));
  while LCurrPos^ = '0' do
  begin
    Inc(PHeader^.NumberOfSymbols);
    with PSymbol^ do
    begin
      SymbolSegId := ReadDecValue;
      Inc(LCurrPos); // Skip":".
      SymbolOffset := ReadHexValue;
      while LCurrPos^ = ' ' do
        Inc(LCurrPos);
      PUnit := GetUnit(SymbolSegId, SymbolOffset);

      L := 0;
      while not((LCurrPos + L)^ in [' ', #13, #10]) do
        Inc(L);
      if Assigned(PUnit) then
        with PUnit^ do
        begin
          Inc(LCurrPos, UnitNameLength + 1);
          Dec(L, UnitNameLength + 1);
        end;
      Move(LCurrPos^, Pointer(@SymbolName[0])^, L);
      SymbolNameLength := L;
    end;
    NextBeginOfLine;
    Inc(PSymbol);
    Inc(PByte(PSymbol), L);
  end;

  PSrc := PSMapSourceLocation(PSymbol);
  PHeader^.OffsetToSourceLocations := Cardinal(NativeUInt(PSrc) - NativeUInt(PHeader));

  while (LCurrPos^ = 'L') // Limit ReachedHint call !
    and (ReachedHint(HintLineNumbersArray)) do
  begin
    Inc(PHeader^.NumberOfSourceLocations);
    while LCurrPos^ <> '(' do
      Inc(LCurrPos);
    Inc(LCurrPos);
    L := 0;
    while (LCurrPos + L)^ <> ')' do
      Inc(L);
    Move(LCurrPos^, Pointer(@PSrc^.SourceLocation[0])^, L);
    Inc(LCurrPos, L);
    PSrc^.SourceLocationLength := L;

    PLine := PSMapLineNumber(PByte(PSrc) + SizeOf(TSMapSourceLocation) + L);
    NextBeginOfLine;
    while LCurrPos^ in ['0' .. '9'] do
    begin
      Inc(PSrc^.NumberOfLineNumbers);
      with PLine^ do
      begin
        LineNumber := ReadDecValue;
        while LCurrPos^ = ' ' do
          Inc(LCurrPos);
        PSrc^.SegId := ReadDecValue;
        Inc(LCurrPos); // Skip ":"
        Offset := ReadHexValue;
      end;
      Inc(PLine);
      while LCurrPos^ = ' ' do
        Inc(LCurrPos);
      if LCurrPos^ in [#13, #10] then
        NextBeginOfLine;
    end;
    PSrc := PSMapSourceLocation(PLine);
  end;
  FreeMem(PSegsUnits);
  PTmpHeader := PHeader;
  { Skip header => We don't want to compress smap header. }
  Inc(PHeader);
  { Calculate new smap size before compression. }
  Result := Integer(NativeUInt(PLine) - NativeUInt(PHeader));
  PTmpHeader^.Size := Result + SizeOf(TSMapHeader);
  if moCompress in Options then
  begin
    { Compress smap. }
    System.ZLib.ZCompress(PHeader, Result, Buffer, Result);
    { Copy compressed data to smap. }
    Move(Buffer^, PHeader^, Result);
    FreeMem(Buffer);
  end;
  { Set size of compressed smap. }
  Inc(Result, SizeOf(TSMapHeader));
  PTmpHeader^.cSize := Result;
  { Align size => Result is always 4 bytes aligned. }
  Result := ((Result + 3) div 4) * 4;
end;

function ConvertMapToSMap(const MapFile: string; const Options: TSMapOptions = [moCompress]): Integer;
var
  hSrcFile: THandle;
  hDstFile: THandle;
  SrcMapObj: THandle;
  DstMapObj: THandle;
  SrcMapAddress: Pointer;
  DstMapAddress: Pointer;
  NewFileName: string;
  fs: Cardinal;
begin
  Result := 0;
  if not FileExists(MapFile) then
    Exit;
  hSrcFile := CreateFile(PChar(MapFile), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hSrcFile <> INVALID_HANDLE_VALUE then
  begin
    fs := GetFileSize(hSrcFile, nil);
    SrcMapObj := CreateFileMapping(hSrcFile, nil, PAGE_READONLY, 0, 0, nil);
    if SrcMapObj <> 0 then
    begin
      SrcMapAddress := MapViewOfFile(SrcMapObj, FILE_MAP_READ, 0, 0, 0);
      if Assigned(SrcMapAddress) then
      begin
        NewFileName := ChangeFileExt(MapFile, SMapFileExtension);
        hDstFile := CreateFile(PChar(NewFileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
        if hDstFile <> INVALID_HANDLE_VALUE then
        begin
          DstMapObj := CreateFileMapping(hDstFile, nil, PAGE_READWRITE, 0, fs, nil);
          if DstMapObj <> 0 then
          begin
            DstMapAddress := MapViewOfFile(DstMapObj, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
            if Assigned(DstMapAddress) then
            begin
              Result := ConvertMapToSMap(SrcMapAddress, DstMapAddress, Options);
              UnmapViewOfFile(DstMapAddress);
            end;
            CloseHandle(DstMapObj);
          end;
          if Result > 0 then
          begin
            SetFilePointer(hDstFile, Result, nil, FILE_BEGIN);
            SetEndOfFile(hDstFile);
          end;
          CloseHandle(hDstFile);
        end;
        UnmapViewOfFile(SrcMapAddress);
      end;
      CloseHandle(SrcMapObj);
    end;
    CloseHandle(hSrcFile);
  end;
end;

function ConvertMapToSMap(SrcStream, DstStream: TMemoryStream; Options: TSMapOptions): Integer;
begin
  if (not Assigned(SrcStream)) or (not Assigned(DstStream)) then
    Exit(0);
  if DstStream.Size < SrcStream.Size then
    DstStream.SetSize(SrcStream.Size);
  Result := ConvertMapToSMap(SrcStream.Memory, DstStream.Memory, Options);
  DstStream.SetSize(Result);
  DstStream.Seek(0, soEnd);
end;

{$ENDREGION 'MapConverter'}
{$REGION 'DebugInfo'}
{ TModule }

constructor TModule.Create(ModuleHandle: THandle);
begin
  FModule := ModuleHandle;
  FStartAddress := nil;
  FEndAddress := nil;
  FDebugInfo := nil;
  FSize := 0;
  FBaseName := EmptyStr;
  FFileName := EmptyStr;
  FMapLocation := mlNone;

  InitializeModule;
  CreateDebugInfo;
end;

procedure TModule.CreateDebugInfo;
var
  MapFileName: string;
  NtHeaders: PImageNtHeaders;
  Section: PImageSectionHeader;
  MS: TMemoryStream;
  P: Pointer;
  ResInfo: HRSRC;
  ResSize: DWORD;
  hRes: HGLOBAL;
begin
  case FMapLocation of
    mlDisk:
      begin
        { Load from disk }
        MapFileName := ChangeFileExt(FBaseName, DelphiMapFileExtension);
        ConvertMapToSMap(MapFileName, []);
        MapFileName := ChangeFileExt(FBaseName, SMapFileExtension);
        FDebugInfo := TDebugInfoSMap.Create(Self);
        TDebugInfoSMap(FDebugInfo).LoadFromFile(MapFileName);
      end;
    mlSection:
      begin
        { Load from SDEBUG section. }
        NtHeaders := PeMapImageNtHeaders(Pointer(FModule));
        Section := PeFindSection(NtHeaders, SDebugSection);
        P := Pointer(NativeUInt(FModule) + Section^.VirtualAddress);
        FDebugInfo := TDebugInfoSMap.Create(Self);
        MS := TMemoryStream.Create;
        try
          MS.Write(P^, Section^.Misc.VirtualSize);
          MS.Seek(0, soFromBeginning);
          TDebugInfoSMap(FDebugInfo).LoadFromStream(MS);
        finally
          MS.Free;
        end;
      end;
    mlResource:
      begin
        { Load from resource. }
        ResInfo := FindResource(FModule, SMapResName, SMapResType);
        ResSize := SizeofResource(FModule, ResInfo);
        hRes := LoadResource(FModule, ResInfo);
        P := LockResource(hRes);
        FDebugInfo := TDebugInfoSMap.Create(Self);
        MS := TMemoryStream.Create;
        try
          MS.Write(P^, ResSize);
          MS.Seek(0, soFromBeginning);
          TDebugInfoSMap(FDebugInfo).LoadFromStream(MS);
        finally
          MS.Free;
        end;
      end;
    mlNone:
      begin
        { There is no map => Get info from export directory }
        FDebugInfo := TDebugInfoExport.Create(Self);
      end;
  end;
end;

destructor TModule.Destroy;
begin
  if Assigned(FDebugInfo) then
    FDebugInfo.Free;
  inherited;
end;

function TModule.GetImageBase: NativeUInt;
var
  NtHeaders: PImageNtHeaders;
begin
  NtHeaders := PeMapImageNtHeaders(Pointer(FModule));
  Result := NtHeaders^.OptionalHeader.ImageBase;
end;

function TModule.GetIsBorlandModule: Boolean;
var
  CurModule: PLibModule;
begin
  CurModule := System.LibModuleList;
  while Assigned(CurModule) do
  begin
    if (CurModule^.Instance = FModule) then
      Exit(True);
    CurModule := CurModule^.Next;
  end;
  Result := False;
end;

function TModule.GetMapLocation: TMapLocation;
var
  MapFileName: string;
begin
  { SDEBUG }
  if Assigned(PeFindSection(PeMapImageNtHeaders(Pointer(FModule)), SDebugSection)) then
    Exit(mlSection);

  { SMAP }
  if FindResource(FModule, SMapResName, SMapResType) <> 0 then
    Exit(mlResource);

  { Delphi map }
  { Leave this test the last one. }
  MapFileName := ChangeFileExt(FBaseName, DelphiMapFileExtension);
  if FileExists(MapFileName) then
    Exit(mlDisk);

  { No map associated with module }
  Result := mlNone;
end;

function TModule.IsAddressInModuleRange(Address: Pointer): Boolean;
begin
  Result := IsAddressInRange(Address, FStartAddress, FEndAddress);
end;

procedure TModule.InitializeModule;
var
  NtHeaders: PImageNtHeaders;
begin
  FStartAddress := Pointer(FModule);
  NtHeaders := PeMapImageNtHeaders(Pointer(FModule));
  if Assigned(NtHeaders) then
  begin
    FSize := NtHeaders^.OptionalHeader.SizeOfImage;
    FEndAddress := Pointer(NativeUInt(FStartAddress) + FSize);
    FBaseName := GetModuleBaseName(FModule);
    FFileName := GetModuleFileName(FModule);
    FBorlandModule := GetIsBorlandModule;
    FMapLocation := GetMapLocation;
  end;
end;

{ TDebugInfoBase }

constructor TDebugInfoBase.Create(Module: TModule);
begin
  FModule := Module;
end;

destructor TDebugInfoBase.Destroy;
begin

  inherited;
end;

{ TDebugInfoMapBase }

constructor TDebugInfoMapBase.Create(Module: TModule);
begin
  inherited;
  FMapStream := TMemoryStream.Create;
end;

destructor TDebugInfoMapBase.Destroy;
begin
  FMapStream.Free;
  inherited;
end;

function TDebugInfoMapBase.LoadFromFile(const MapFileName: string): Boolean;
var
  LStream: TFileStream;
begin
  Result := FileExists(MapFileName);
  if Result then
  begin
    LStream := TFileStream.Create(MapFileName, fmOpenRead);
    try
      Result := LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;
end;

function TDebugInfoMapBase.LoadFromStream(Stream: TStream): Boolean;
begin
  FMapStream.Clear;
  FMapStream.LoadFromStream(Stream);
  Result := FMapStream.Size > 1;
  if Result then
    Result := ProcessMap;
end;

{ TDebugInfoSMap }

constructor TDebugInfoSMap.Create(Module: TModule);
begin
  inherited;
  FRTSegments := TList.Create;
  FUnits := TList.Create;
  FSymbols := TList.Create;
  FLineSources := TList.Create;
end;

destructor TDebugInfoSMap.Destroy;
var
  I: Integer;
begin
  for I := 0 to FRTSegments.Count - 1 do
    if Assigned(FRTSegments[I]) then
      FreeMem(FRTSegments[I]);

  FRTSegments.Free;
  FUnits.Free;
  FSymbols.Free;

  for I := 0 to FLineSources.Count - 1 do
    if Assigned(FLineSources[I]) then
      Dispose(FLineSources[I]);
  FLineSources.Free;

  inherited;
end;

function TDebugInfoSMap.GetAddressFromIndex(Index: Integer): Pointer;
var
  PSymbol: PSMapSymbol;
  PRtSegment: PRuntimeSegment;
begin
  if (Index > -1) and (Index < FSymbols.Count) then
  begin
    PSymbol := FSymbols[Index];
    PRtSegment := GetRTSegmentFromSegIndex(PSymbol^.SymbolSegId);
    if not Assigned(PRtSegment) then
      Exit(nil);
    Result := Pointer(NativeUInt(PRtSegment^.SegStartAddress) + PSymbol^.SymbolOffset);
    Exit;
  end;
  Result := nil;
end;

function TDebugInfoSMap.GetAddressInfo(Address: Pointer; out Info: TAddressInfo; Mask: TAddressInfoMask): Boolean;
var
  PRtSeg: PRuntimeSegment;
  PSymbol: PSMapSymbol;
  PUnit: PSMapUnit;
  PLineSource: PLineNumberSource;
  PSourceLocation: PSMapSourceLocation;
  SymbolAddress: Pointer;
  I: Integer;
begin
  Result := False;
  PRtSeg := GetRTSegmentFromAddress(Address);
  if not Assigned(PRtSeg) then
    Exit;
  for I := FSymbols.Count - 1 downto 0 do
  begin
    PSymbol := FSymbols[I];
    if PSymbol^.SymbolSegId = PRtSeg^.SegId then
    begin
      SymbolAddress := Pointer(PByte(PRtSeg^.SegStartAddress) + PSymbol^.SymbolOffset);
      if (NativeUInt(SymbolAddress) < NativeUInt(PRtSeg^.SegEndAddress)) and (NativeUInt(Address) >= NativeUInt(SymbolAddress)) then
      begin
        Result := True;
        FillChar(Info, SizeOf(Info), #00);
        Info.DebugSource := Self;
        Info.SymbolAddress := SymbolAddress;
        Info.SymbolIndex := I;
        if Mask = aimAddress then
          Exit;
        Info.SymbolName := string(PSMapChar(@PSymbol^.SymbolName[0]));
        SetLength(Info.SymbolName, PSymbol^.SymbolNameLength);
        PUnit := GetUnit(SymbolAddress, PRtSeg);
        if Assigned(PUnit) then
        begin
          Info.UnitName := string(PSMapChar(@PUnit^.UnitName[0]));
          SetLength(Info.UnitName, PUnit^.UnitNameLength);
        end;
        if Mask = aimSymbolName then
          Exit;
        PLineSource := GetLineNumberSource(Address, PRtSeg);
        if Assigned(PLineSource) then
        begin
          Info.LineNumber := PLineSource^.Line^.LineNumber;
          PSourceLocation := PLineSource^.Source;
          Info.SourceLocation := string(PSMapChar(@PSourceLocation^.SourceLocation[0]));
          SetLength(Info.SourceLocation, PSourceLocation^.SourceLocationLength);
        end;
        Exit;
      end;
    end;
  end;
end;

function TDebugInfoSMap.GetLineNumberSource(Address: Pointer; PRtSeg: PRuntimeSegment): PLineNumberSource;
var
  LineNumberAddress: NativeUInt;
  I: Integer;
begin
  for I := FLineSources.Count - 1 downto 0 do
  begin
    Result := FLineSources[I];
    if Result^.Source^.SegId = PRtSeg^.SegId then
    begin
      LineNumberAddress := NativeUInt(PRtSeg^.SegStartAddress) + Result^.Line^.Offset;
      if NativeUInt(Address) >= NativeUInt(LineNumberAddress) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TDebugInfoSMap.GetRTSegmentFromAddress(Address: Pointer): PRuntimeSegment;
var
  I: Integer;
begin
  for I := 0 to FRTSegments.Count - 1 do
  begin
    Result := FRTSegments[I];
    with Result^ do
      if Assigned(Result) and IsAddressInRange(Address, SegStartAddress, SegEndAddress) then
        Exit;
  end;
  Result := nil;
end;

function TDebugInfoSMap.GetRTSegmentFromSegIndex(SegIndex: Integer): PRuntimeSegment;
var
  I: Integer;
begin
  for I := 0 to FRTSegments.Count - 1 do
  begin
    Result := FRTSegments[I];
    if Assigned(Result) and (Result^.SegId = Cardinal(SegIndex)) then
      Exit;
  end;
  Result := nil;
end;

function TDebugInfoSMap.GetSymbolAddress(const UnitName, SymbolName: string): Pointer;
var
  I: Integer;
  PSymbol: PSMapSymbol;
  S: string;
  PUnit: PSMapUnit;
  RtSeg: PRuntimeSegment;
begin
  Result := nil;
  for I := 0 to FSymbols.Count - 1 do
  begin
    PSymbol := FSymbols[I];
    S := string(PSMapChar(@PSymbol^.SymbolName[0]));
    if SameText(S, SymbolName) then
    begin
      RtSeg := GetRTSegmentFromSegIndex(PSymbol^.SymbolSegId);
      if Assigned(RtSeg) then
      begin
        Result := Pointer(NativeUInt(RtSeg^.SegStartAddress) + PSymbol^.SymbolOffset);
        if UnitName <> EmptyStr then
        begin
          PUnit := GetUnit(Result, RtSeg);
          if Assigned(PUnit) then
          begin
            S := string(PSMapChar(@PUnit^.UnitName[0]));
            if not SameText(S, UnitName) then
            begin
              Result := nil;
              Continue;
            end;
          end;
        end;
      end;
      Exit;
    end;
  end;
end;

function TDebugInfoSMap.GetUnit(Address: Pointer; PRtSeg: PRuntimeSegment): PSMapUnit;
var
  I: Integer;
begin
  for I := 0 to FUnits.Count - 1 do
  begin
    Result := FUnits[I];
    with Result^, PRtSeg^ do
      if (UnitSegId = SegId) and IsAddressInRange(Address, Pointer(NativeUInt(SegStartAddress) + UnitOffset), Pointer(NativeUInt(SegStartAddress) + UnitOffset + UnitLength)) then
        Exit;
  end;
  Result := nil;
end;

function LineSortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := NativeUInt(PLineNumberSource(Item1)^.Source^.SegId) - NativeUInt(PLineNumberSource(Item2)^.Source^.SegId);
  if Result = 0 then
    Result := NativeUInt(PLineNumberSource(Item1)^.Line^.Offset) - NativeUInt(PLineNumberSource(Item2)^.Line^.Offset);
end;

function TDebugInfoSMap.ProcessMap: Boolean;
var
  LModuleImageBase: NativeUInt;
  J: Integer;
  function RegisterSegment(PSegment: PSMapSegment): PRuntimeSegment;
  begin
    { Convert segment to run time segment and register it. }
    GetMem(Result, SizeOf(TRuntimeSegment));
    Result^.SegId := PSegment^.SegId;
    Result^.SegLength := PSegment^.SegLength;
    { Module could be loaded at a different address space
      specified by Delphi. So we need to fix segment start address. }
    Result^.SegStartAddress := Pointer((PSegment^.SegStartAddress - LModuleImageBase) + FModule.ModuleHandle);
    Result^.SegEndAddress := Pointer(PByte(Result^.SegStartAddress) + Result^.SegLength);

    if not FModule.IsAddressInModuleRange(Result^.SegStartAddress) then
    begin
      FreeMem(Result);
      Result := nil;
    end
    else
      FRTSegments.Add(Result);
  end;

var
  PHeader: PSMapHeader;
  PSegment: PSMapSegment;
  PUnit: PSMapUnit;
  PSymbol: PSMapSymbol;
  PSource: PSMapSourceLocation;
  PLine: PSMapLineNumber;
  PLineSource: PLineNumberSource;
  I: Integer;
  Options: TSMapOptions;
begin
  LModuleImageBase := FModule.ImageBase;
  PHeader := FMapStream.Memory;
  if PHeader^.Signature = SMapSignature then
  begin
    Options := RawToSMapOptions(PHeader^.Flags);
    if (moCompress in Options) and (not UnZip) then
      Exit(False);

    PHeader := FMapStream.Memory; // Memory pointer may get changed !
    PSegment := PSMapSegment(PByte(PHeader) + SizeOf(TSMapHeader));
    PUnit := PSMapUnit(PByte(PHeader) + PHeader^.OffsetToUnits);
    PSymbol := PSMapSymbol(PByte(PHeader) + PHeader^.OffsetToSymbols);
    PSource := PSMapSourceLocation(PByte(PHeader) + PHeader^.OffsetToSourceLocations);
    PLine := PSMapLineNumber(PByte(PSource) + SizeOf(TSMapSourceLocation) + PSource^.SourceLocationLength);

    { ===> Segments <=== }
    for I := 1 to PHeader^.NumberOfSegments do
    begin
      RegisterSegment(PSegment);
      Inc(PSegment);
    end;
    { ===> Units <=== }
    for I := 1 to PHeader^.NumberOfUnits do
    begin
      FUnits.Add(PUnit);
      PUnit := PSMapUnit(PByte(PUnit) + SizeOf(TSMapUnit) + PUnit^.UnitNameLength);
    end;
    { ===> Symbols <=== }
    for I := 1 to PHeader^.NumberOfSymbols do
    begin
      FSymbols.Add(PSymbol);
      PSymbol := PSMapSymbol(PByte(PSymbol) + SizeOf(TSMapSymbol) + PSymbol^.SymbolNameLength);
    end;
    { ===> SourceLocations & LineNumber <=== }
    for I := 1 to PHeader^.NumberOfSourceLocations do
    begin
      for J := 1 to PSource^.NumberOfLineNumbers do
      begin
        New(PLineSource);
        PLineSource^.Line := PLine;
        PLineSource^.Source := PSource;
        FLineSources.Add(PLineSource);
        Inc(PLine);
      end;
      PSource := Pointer(PLine);
      PLine := PSMapLineNumber(PByte(PSource) + SizeOf(TSMapSourceLocation) + PSource^.SourceLocationLength);
    end;
    FLineSources.Sort(LineSortCompare);
  end;
  Result := True;
end;

function TDebugInfoSMap.UnZip: Boolean;
var
  SMapHeader: TSMapHeader;
  P: Pointer;
  Buffer: Pointer;
  BufferSize: Integer;
begin
  P := FMapStream.Memory;
  { Save smap header. }
  SMapHeader := PSMapHeader(P)^;
  { We don't want to decompress smap header cause it was
    not compressed. => So Skip it. }
  Inc(PByte(P), SizeOf(TSMapHeader));
  { Decompress }
  ZDecompress(P, SMapHeader.cSize - SizeOf(TSMapHeader), Buffer, BufferSize);
  { Restore smap header. }
  FMapStream.Write(Pointer(@SMapHeader)^, SizeOf(TSMapHeader));
  { Copy decompressed data. }
  FMapStream.Write(Buffer^, BufferSize);
  FreeMem(Buffer);
  { Check if size of decompressed data = size of original smap before it gets compressed. }
  Inc(BufferSize, SizeOf(TSMapHeader));
  Result := Integer(SMapHeader.Size) = BufferSize;
  // ZDecompress uses size as signed !
end;

{ TDebugInfoExport }

function ExportFunctionsSortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := NativeUInt(TDebugInfoExport.PExportInfo(Item1)^.Address) - NativeUInt(TDebugInfoExport.PExportInfo(Item2)^.Address);
end;

constructor TDebugInfoExport.Create(Module: TModule);
begin
  inherited;
  FExportList := TList.Create;
  CreateExportList;
end;

destructor TDebugInfoExport.Destroy;
var
  I: Integer;
  P: Pointer;
begin
  for I := 0 to FExportList.Count - 1 do
  begin
    P := FExportList[I];
    if Assigned(P) then
    begin
      // FinalizeRecord(P, TypeInfo(TExportInfo));
      Finalize(PExportInfo(P)^);
      Dispose(FExportList[I]);
    end;
  end;
  FExportList.Free;
  inherited;
end;

procedure TDebugInfoExport.CreateExportList;
  function RvaToVa(Rva: Cardinal): Pointer;
  begin
    { All modules are loaded into the process address space ! }
    Result := Pointer(Rva + FModule.ModuleHandle);
  end;

var
  NtHeaders: PImageNtHeaders;
  EntryData: PImageExportDirectory;
  Info: PExportInfo;
  POrd: PWord;
  P: PDWORD;
  ExportSize: DWORD;
  nFunctions: DWORD;
  nNames: DWORD;
  I: Integer;
begin
  NtHeaders := PeMapImageNtHeaders(Pointer(FModule.ModuleHandle));
  ExportSize := NtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
  EntryData := ImageDirectoryEntryToData(Pointer(FModule.ModuleHandle), True, IMAGE_DIRECTORY_ENTRY_EXPORT, ExportSize);

  if not Assigned(EntryData) then
    Exit;

  with EntryData^ do
  begin
    nFunctions := NumberOfFunctions;
    nNames := NumberOfNames;
    POrd := RvaToVa(AddressOfNameOrdinals);
    P := RvaToVa(AddressOfFunctions);
  end;

  { Functions address. }
  for I := 0 to nFunctions - 1 do
  begin
    // Don't forget to dispose info when destroying !
    New(Info);
    with Info^ do
    begin
      Hint := I; // In case there is no name.
      Address := RvaToVa(P^);
      Ord := EntryData^.Base + Word(I);
      Name := EmptyStr;
    end;
    Inc(P);
    FExportList.Add(Info);
  end;

  { Functions names. }
  P := RvaToVa(EntryData^.AddressOfNames);
  for I := 0 to nNames - 1 do
  begin
    Info := FExportList[POrd^];
    Info^.Name := string(PAnsiChar(RvaToVa(P^)));
    Inc(P);
    Inc(POrd);
  end;

  { We must sort exported functions by address value. }
  FExportList.Sort(ExportFunctionsSortCompare);
end;

function TDebugInfoExport.GetAddressFromIndex(Index: Integer): Pointer;
begin
  if (Index > -1) and (Index < FExportList.Count) then
  begin
    Result := PExportInfo(FExportList[Index])^.Address;
    Exit;
  end;
  Result := nil;
end;

function TDebugInfoExport.GetAddressInfo(Address: Pointer; out Info: TAddressInfo; Mask: TAddressInfoMask): Boolean;
var
  I: Integer;
  ExportInfo: PExportInfo;
begin
  if FModule.IsAddressInModuleRange(Address) then
  begin
    for I := FExportList.Count - 1 downto 0 do
    begin
      ExportInfo := FExportList[I];
      if (NativeUInt(Address) >= NativeUInt(ExportInfo^.Address)) then
      begin
        Result := True;
        FillChar(Info, SizeOf(Info), #00);
        Info.DebugSource := Self;
        Info.SymbolAddress := ExportInfo^.Address;
        Info.SymbolIndex := I;
        if Mask = aimAddress then
          Exit;
        if ExportInfo^.Name.IsEmpty then
          Info.SymbolName := ChangeFileExt(FModule.FBaseName, '.') + IntToStr(ExportInfo^.Hint)
        else
          Info.SymbolName := ExportInfo^.Name;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

function TDebugInfoExport.GetSymbolAddress(const UnitName, SymbolName: string): Pointer;
var
  I: Integer;
  PExport: PExportInfo;
begin
  for I := 0 to FExportList.Count - 1 do
  begin
    PExport := FExportList[I];
    if SameText(PExport^.Name, SymbolName) then
      Exit(PExport^.Address);
  end;
  Result := nil;
end;

{ TModules }

constructor TModules.Create;
begin
  FModulesList := TList.Create;
end;

destructor TModules.Destroy;
var
  I: Integer;
begin
  for I := 0 to FModulesList.Count - 1 do
    TObject(FModulesList[I]).Free;
  FModulesList.Free;
  inherited;
end;

function TModules.AddModule(ModuleHandle: THandle): TModule;
begin
  Result := TModule.Create(ModuleHandle);
  FModulesList.Add(Result);
end;

function TModules.GetModuleFromAddress(Address: Pointer): TModule;
var
  ModuleHandle: THandle;
begin
  ModuleHandle := GetModuleHandleFromAddress(Address);
  Result := GetModuleFromModuleHandle(ModuleHandle, True);
end;

function TModules.GetModuleFromModuleHandle(ModuleHandle: THandle; RegisterNoExists: Boolean): TModule;
  function GetModule: TModule;
  var
    I: Integer;
  begin
    for I := 0 to FModulesList.Count - 1 do
    begin
      Result := FModulesList[I];
      if Result.ModuleHandle = ModuleHandle then
        Exit;
    end;
    Result := nil;
  end;

begin
  if ModuleHandle = 0 then
    Exit(nil);

  Result := GetModule;
  if Assigned(Result) then
    Exit;

  TMonitor.Enter(GlobalLock);
  try
    { Try again => Maybe another thread had already registered the current module. }
    Result := GetModule;
    if Assigned(Result) then
      Exit;
    if RegisterNoExists then
      Result := AddModule(ModuleHandle);
  finally
    TMonitor.Exit(GlobalLock);
  end;
end;

{$ENDREGION 'DebugInfo'}

initialization

{$IFDEF DEVMODE}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF DEVMODE}
GlobalLock := TObject.Create;
RegxLock := TObject.Create;

finalization

if Assigned(GlobalLock) then
  GlobalLock.Free;

if Assigned(RegxLock) then
  RegxLock.Free;

if Assigned(GlobalModules) then
  GlobalModules.Free;

end.
