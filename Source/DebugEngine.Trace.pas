// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.Trace
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.Trace.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

unit DebugEngine.Trace;

interface

{ TODO:
  - Support call's line number. => DONE.
}

{$I DebugEngine.inc}
{ .$DEFINE DEVMODE }

uses
  WinApi.Windows,
  System.SysUtils,
  System.Classes,
  DebugEngine.Core,
  DebugEngine.DebugInfo;

type
{$IFDEF STACK_BASED_EXCEPTIONS}
  PExceptionFrame = ^TExceptionFrame;

  TExceptionFrame = packed record
    { All those data are stored into the stack. }
    { Next frame : push dword ptr fs:[eax] }
    Next: PExceptionFrame;
    { Target : push offset }
    Target: Pointer;
    { EBP frame : push ebp }
    EBP: Pointer;
  end;

  TStackFrame = packed record
    CallerFrame: Pointer;
    ReturnAddress: Pointer;
  end;

  PStackFrame = ^TStackFrame;

{$ENDIF STACK_BASED_EXCEPTIONS}
  { * means default option. }

  { ===> TStackTraceMethod <===

    -stRaw = Raw stack trace.

    -stFrames = EBP frames stack trace.
    All calls are guaranteed to be a part of ebp chain.

    -*stCombined = Do a raw trace based on ebp frames.
    This is the default option. And I strongly recommend to use. }

  TStackTraceMethod = (stRaw, stFrames, stCombined);

  { ===> TStackTraceFilterOptions <===

    -*sfExcludeTryBlocks = Remove the try blocks garbage data from the stack.

    -*sfUnwind = Unwind the stack.

    -sfNoTolerance = The filter will run several times to eliminate as much as it can.

    -*sfHideOutOfRangeCalls = Hide calls that their address does not have a proper address.

    -*sfHideUnknownCallsInfo = Hide calls that do not have address,symbol name information.
  }
  TStackTraceFilterOptions = set of (sfExcludeTryBlocks, sfUnwind, sfNoTolerance, sfHideOutOfRangeCalls, sfHideUnknownCallsInfo);

  { ===> TStackOptions <===
    -*soUseFirstCallOnEbp = Make esp equal to ebp before tracing.
    The first call is guaranteed to be a part of ebp chain.

    -soRebuildBrokenEbpChain = Try to find the broken ebp chain and use this chain to help the filter.

    -soDropCurrentEbpChain = if soRebuildBrokenEbpChain is used and the trace detected a broken chain,
    it will drop the current ebp chain and use the broken chain instead.
  }
  TStackOptions = set of (soUseFirstCallOnEbp, soRebuildBrokenEbpChain, soDropCurrentEbpChain);

  TMiniDebugInfo = record
    Address: Pointer;
    ExportIndex: Integer;
    ProcedureAddress: Pointer;
    DebugSource: TDebugInfoBase;
  end;

  PMiniDebugInfo = ^TMiniDebugInfo;

  PStackItem = ^TStackItem;

  // x64 compatibility.
{$HINTS OFF}

  TStackItem = record
  private
    function IsValid: Boolean;
    function OutOfRange: Boolean;
    function InEbpFrames: Boolean;
    function NoInfo: Boolean;
    function Kill: Boolean;
  public
  var
    CallAddress: Pointer;
    Rank: UInt8;
    Info: TMiniDebugInfo;
    cIndex: Integer;
{$IF defined(CPUX86) or defined(DEVMODE)}
    iIndex: Integer;
{$ENDIF}
    case { Trace: } Integer of
{$IFDEF TABLE_BASED_EXCEPTIONS}
      { x64 call trace. }
      0: (ReturnAddress: Pointer);
{$ELSE !TABLE_BASED_EXCEPTIONS}
      { Exception trace. }
      0: (ExceptionFrame: PExceptionFrame);
{$ENDIF TABLE_BASED_EXCEPTIONS}
      { x86-32 call trace. }
      { PPointer(StackPtr)^ = ReturnAddress }
      1: (StackPtr: Pointer;
          case { CallTrace: } Boolean of
{$IFDEF STACK_BASED_EXCEPTIONS}
            False: (ExceptionHandler: Pointer);
{$ENDIF STACK_BASED_EXCEPTIONS}
            True: (CallTargetAddress: Pointer;
              case { TryTrace: } Boolean of
                False: (Flags: UInt8);
{$IFDEF STACK_BASED_EXCEPTIONS}
                True: (TryType: TTryType);
{$ENDIF STACK_BASED_EXCEPTIONS}
                );
          );

  end;

  TStackTrace = class(TObject)
  private
    FTracedItems: TList;
    FStackInfo: TStackInfo;
    FIsStackReadable: Boolean;
    FTraceMethod: TStackTraceMethod;
    FFilter: Boolean;
    FFilterOptions: TStackTraceFilterOptions;
    FOptions: TStackOptions;
    FExceptionAddress: Pointer;
    function GetCount: Integer;
    function GetItem(Index: Integer): PStackItem;
    function GetStackSize: Integer;
    function GetIsStackReadable: Boolean;
    procedure SetStackInfo(const StackInfo: TStackInfo);
  protected
    { All descendant classes must implement this function. }
    function CustomTrace: Boolean; virtual; abstract;
    procedure Clean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AcceptAddress(Address: Pointer): Boolean;
    function AcceptReturnAddress(Address: Pointer; out CallAddress, CallTargetAddress: Pointer): Boolean; overload;
    function AcceptReturnAddress(Address: Pointer): Boolean; overload;
    function AddressInStackRange(Address: Pointer): Boolean;
    function Trace: Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: PStackItem read GetItem;
    property StackSize: Integer read GetStackSize;
    property StackInfo: TStackInfo read FStackInfo write SetStackInfo;
    property Options: TStackOptions read FOptions write FOptions;
    property Filter: Boolean read FFilter write FFilter;
    property FilterOptions: TStackTraceFilterOptions read FFilterOptions write FFilterOptions;
    property TraceMethod: TStackTraceMethod read FTraceMethod write FTraceMethod;
    property IsStackReadable: Boolean read FIsStackReadable;
  end;

{$IFDEF CPUX86}

  TCallTrace32 = class(TStackTrace)
  private
    FItems: TList;
    FCalls: TList;
    FFrames: TList;
    function GetItemFromStackPtr(StackPtr: Pointer; Index: Integer): PStackItem;
    function GetCallerItem(Item: PStackItem; Index: Integer; Flags: ShortInt): PStackItem;
    function GetFirstTargetItem(Item: PStackItem): PStackItem;
    function IsReturnAddresInEbpFrame(PReturnAddress: PPointer): Boolean;
    function RegionCouldBeDummy(AStart, AEnd: PStackItem): Boolean;
    function GetValidLink(Item: PStackItem): PStackItem;
    function GetItemEbpFrame(Item: PStackItem): PStackFrame;
    function ValidFrame(Frame: PStackFrame): Boolean;
    function GetValidCallsCount: Integer;
  protected
    procedure ReRankItems(Frame: PStackFrame);
    procedure RegisterStackPtr(StackPtr: Pointer);
    procedure CollectRawItems;
    procedure CollectEbpFrames;
    procedure CollectFrameCalls;
    procedure RemoveTryBlocksArea;
    procedure RemoveSealedCalls;
    procedure RemoveCallsBetweenTwoEbpCall;
    procedure UnwindStack(OnlyEbp: Boolean);
    procedure DispatchRank;
    procedure RemoveInBetweenCalls;
    procedure RemoveDuplicateCalls;
    function RebuildBrokenEbpChain(var NewFrame: PStackFrame): Boolean;
    procedure Clean; override;
    function CustomTrace: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{$ELSE !CPUX86}

  TCallTrace64 = class(TStackTrace)
  private
    FCalls: TList;
  protected
    procedure Clean; override;
    function CustomTrace: Boolean; override;
    procedure AddNewItem(ReturnAddress: Pointer);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
{$ENDIF CPUX86}
{$IFDEF STACK_BASED_EXCEPTIONS }

  TTryTrace = class(TStackTrace)
  private
    FHandlersPtr: THandlersPtr;
  protected
    procedure Clean; override;
    function CustomTrace: Boolean; override;
    procedure RegisterFrame(Frame: PExceptionFrame);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$IFDEF CPUX86}

  TCallTrace = TCallTrace32;
{$ELSE !CPUX86}
  TCallTrace = TCallTrace64;
{$ENDIF CPUX86}
  EStackTraceException = class(Exception);

{$HINTS ON}
  { In case you use call trace when error happens,
    this is useful to distinguish between trace
    errors and other errors (RTL,VCL,WINAPI ...).
    If GetStackTraceError returns 0 that means
    a call to StackTrace returned successfully
    without errors. Otherwise, the current error
    was raised by the StackTrace. }

procedure ClearStackTraceError;

procedure SetStackTraceError(Error: Integer);

function GetStackTraceError: Integer;

/// <summary> Trace the current thread call stack.
/// </summary>
/// <param name="SL"> The function will use this list to output call info.
/// </param>
/// <remarks> This is the easy way to perform a stack trace. Use <see cref="TCallTrace"/> to customize the stack trace.
/// </remarks>
procedure StackTrace(SL: TStrings);

/// <summary> Trace the current thread exception frames.
/// </summary>
/// <param name="SL"> The function will use this list to output exception info.
/// </param>
/// <remarks> This is the easy way to perform an exception trace. Use <see cref="TTryTrace"/> to customize the exception trace.
/// <para></para>
/// Note that this function is only available on x86.
/// </remarks>
procedure TraceTryBlocks(SL: TStrings);

function LogCall(PItem: PStackItem): string;

implementation

uses
{$IFDEF CPUX86}
  UnivDisasm.Disasm,
  UnivDisasm.Cnsts,
  UnivDisasm.Cnsts.Instructions,
  UnivDisasm.Cnsts.Regs,
{$ENDIF CPUX86}
  UnivDisasm.Syntax.NilSyntax,
  DebugEngine.Disasm;

resourcestring
  SNullStackSize = 'Stack size is null.';
  SStackNotReadable = 'Stack is not readable.';

const
  { Item rank }
  RANK_NORMAL = 100;
  RANK_HIGH = 200;

  { Item flags }
  SIF_OUT_OF_RANGE = $01;
  SIF_NO_INFO = $02;

  { GetCallerItem flags }
  GCF_ONLY_EBP = $01;
  GCF_RESPECT_ALIAS = $02;
  GCF_RESPECT_RANK = $04;

threadvar TraceError: Integer;

procedure Bp;
begin

end;

{$REGION 'Misc'}
{$IFDEF CPUX86}

type

  TInternalFrameStruct = record
    Frame: PStackFrame;
    Rank: Integer;
  end;

  PInternalFrameStruct = ^TInternalFrameStruct;

function InternalFrameListSort(Item1, Item2: Pointer): Integer;
begin
  Result := PInternalFrameStruct(Item2)^.Rank - PInternalFrameStruct(Item1)^.Rank;
end;

function __IsHaltCall(TargetAddress: Pointer): Boolean;
asm
  cmp   eax, offset System.@Halt0
  sete  al
end;

function IsGetDynaMethodCall(TargetAddress: Pointer): Boolean;
asm
  cmp   eax, offset System.GetDynaMethod
  sete  al
end;

function IsHaltCall(CallAddress: Pointer): Boolean;
var
  Ins: TInstruction;
  Target: Pointer;
begin
  Ins := TInstruction.Create;
  Ins.Arch := CPUX;
  Ins.Syntax := SX_NIL_SYNTAX;
  Ins.Addr := CallAddress;
  Disasm(@Ins);
  Ins.Free;
  Target := Ins.DstAddr.Addr;
  Result := (Ins.InstID = INST_ID_CALL) and (__IsHaltCall(Target));
end;

function GetFunctionParamsSize(Address: Pointer): Integer;
var
  Ins: TInstruction;
  P: Pointer;
begin
  P := Address;
  Result := -1;
  if not Assigned(P) then
    Exit;

  while True do
  begin
    { If function's calling convention is stdcall,
      then params size will be encoded in
      the ret instruction: ret $xxxx. }
    if IsBadCodePtr(P) then
      Break;
    Ins := TInstruction.Create;
    Ins.Arch := CPUX;
    Ins.Syntax := SX_NIL_SYNTAX;
    Ins.Addr := P;
    Disasm(@Ins);
    P := Ins.NextInst;
    Ins.Free;
    case Ins.InstID of
      INST_ID_RET:
        begin
          Exit(Integer(Ins.Arg1.Imm.Value));
        end;
    end;
  end;
end;

function FunctionHasFrame(Address: Pointer): Boolean;
var
  Ins: TInstruction;
  P: Pointer;
  Hint: Integer;
begin
  { Check if a function uses ebp frame.
    push ebp
    and esp,$xxxx <= Optional
    mov ebp,esp }
  P := Address;
  if not Assigned(P) then
    Exit(False);
  Hint := 0;
  while True do
  begin
    if IsBadCodePtr(P) then
      Break;
    Ins := TInstruction.Create;
    Ins.Arch := CPUX;
    Ins.Syntax := SX_NIL_SYNTAX;
    Ins.Addr := P;
    Disasm(@Ins);
    Ins.Free;
    P := Ins.NextInst;
    case Ins.InstID of
      INST_ID_CALL, INST_ID_RET, INST_ID_JMP:
        begin
          Break;
        end;
      INST_ID_PUSH:
        begin
          if (Ins.Arg1.Reg = REG_EBP) then
          begin
            Hint := 1;
            Continue;
          end
          else
            Break;
        end;
      INST_ID_AND:
        begin
          if (Ins.Arg1.Reg = REG_ESP) then
            Continue;
        end;
      INST_ID_MOV:
        begin
          if (Ins.Arg1.Reg = REG_EBP) then
          begin
            if (Ins.Arg2.Reg = REG_ESP) then
            begin
              if Hint <> 1 then
                Break;
              Exit(True);
            end
            else
              Break;
          end;
        end;
    end;
    Hint := 0;
  end;
  Result := False;
end;

{$ENDIF CPUX86}
{$ENDREGION 'Misc'}

procedure ClearStackTraceError;
begin
  TraceError := 0;
end;

procedure SetStackTraceError(Error: Integer);
begin
  TraceError := Error;
end;

function GetStackTraceError: Integer;
begin
  Result := TraceError;
end;

function LogCall(PItem: PStackItem): string;
var
  LInfo: TAddressInfo;
  LBaseName: string;
  LUnitName: string;
  LSymbolName: string;
{$IFDEF DEVMODE}
  LSymbolAddress: Pointer;
{$ELSE !DEVMODE}
  LineNumber: Cardinal;
  RelLineNumber: Integer;
  SLineNumber: string;
  SRelLineNumber: string;
  SourceFile: string;
{$ENDIF DEVMODE}
begin
{$IFDEF DEVMODE}
  LSymbolAddress := nil;
{$ELSE !DEVMODE}
  LineNumber := 0;
  SLineNumber := EmptyStr;
  SRelLineNumber := EmptyStr;
  SourceFile := EmptyStr;
{$ENDIF DEVMODE}
  if GetAddressInfo(PItem^.Info.Address, LInfo {$IFDEF DEVMODE}, aimSymbolName{$ELSE !DEVMODE} {$ENDIF DEVMODE}) then
  begin
    if Assigned(LInfo.DebugSource) and Assigned(LInfo.DebugSource.Module) then
      LBaseName := LInfo.DebugSource.Module.BaseName
    else
      LBaseName := '??';
    LUnitName := LInfo.UnitName;
    LSymbolName := LInfo.SymbolName;
{$IFDEF DEVMODE}
    LSymbolAddress := LInfo.SymbolAddress;
{$ELSE !DEVMODE}
    LineNumber := LInfo.LineNumber;
    SourceFile := LInfo.SourceLocation;
{$ENDIF DEVMODE}
  end else begin
    LBaseName := '??';
    LUnitName := '??';
    LSymbolName := '??';
  end;

{$IFDEF DEVMODE}
  Result := Format('%d(%d): %d $%p: $%p ($%p) : %s ($%p)', [PItem^.cIndex, PItem^.iIndex, PItem^.Rank, //
    PItem^.StackPtr, PItem^.CallAddress, PItem^.CallTargetAddress, LSymbolName, LSymbolAddress]);
  if PItem^.OutOfRange then
    Result := Result + '**';
{$ELSE !DEVMODE}
  if LineNumber > 0 then
  begin
    SLineNumber := IntToStr(LineNumber);
    if GetAddressInfo(PItem^.Info.ProcedureAddress, LInfo) then
    begin
      RelLineNumber := LineNumber - LInfo.LineNumber;
      SRelLineNumber := '+' + IntToStr(RelLineNumber);
    end;
  end;
  Result := Format('$%p    %s    %s    %s    %s    %s    %s', [PItem^.CallAddress, LBaseName, LUnitName, SourceFile, SLineNumber, SRelLineNumber, LSymbolName]);
{$ENDIF DEVMODE}
end;

{$IFDEF STACK_BASED_EXCEPTIONS}

function LogTryBlock(PItem: PStackItem): string;
var
  LInfo: TAddressInfo;
  LBaseName: string;
  LUnitName: string;
  LSymbolName: string;
begin
  if GetAddressInfo(PItem^.Info.Address, LInfo, aimSymbolName) then
  begin
    if Assigned(LInfo.DebugSource) and Assigned(LInfo.DebugSource.Module) then
      LBaseName := LInfo.DebugSource.Module.BaseName
    else
      LBaseName := '??';
    LUnitName := LInfo.UnitName;
    LSymbolName := LInfo.SymbolName;
  end else begin
    LBaseName := '??';
    LUnitName := '??';
    LSymbolName := '??';
  end;
  Result := Format('$%p    %s    %s    %s', [PItem^.ExceptionFrame^.Target, LBaseName, LUnitName, LSymbolName])
end;
{$ENDIF STACK_BASED_EXCEPTIONS}

procedure StackTrace(SL: TStrings);
var
  StackInfo: TStackInfo;
  Stack: TCallTrace;
  PItem: PStackItem;
  I: Integer;
begin
  GetStackInfo(StackInfo);
  Stack := TCallTrace.Create;
  Stack.SetStackInfo(StackInfo);
  SL.BeginUpdate;
  try
    Stack.Trace;
    for I := 0 to Stack.Count - 1 do
    begin
      PItem := Stack.Items[I];
      SL.Add(LogCall(PItem));
    end;
  finally
    SL.EndUpdate;
    Stack.Free;
  end;
end;

procedure TraceTryBlocks(SL: TStrings);
{$IFDEF STACK_BASED_EXCEPTIONS}
var
  StackInfo: TStackInfo;
  TryTrace: TTryTrace;
  I: Integer;
{$ENDIF STACK_BASED_EXCEPTIONS}
begin
{$IFDEF STACK_BASED_EXCEPTIONS}
  GetStackInfo(StackInfo);
  TryTrace := TTryTrace.Create;
  TryTrace.SetStackInfo(StackInfo);
  SL.BeginUpdate;
  try
    TryTrace.Trace;
    for I := 0 to TryTrace.Count - 1 do
    begin
      SL.Add(LogTryBlock(TryTrace.Items[I]));
    end;
  finally
    SL.EndUpdate;
    TryTrace.Free;
  end;
{$ENDIF STACK_BASED_EXCEPTIONS}
end;

{$REGION 'TStackItem'}
{ TStackItem }

function TStackItem.InEbpFrames: Boolean;
begin
  Result := Rank = RANK_HIGH;
end;

function TStackItem.IsValid: Boolean;
begin
  Result := Rank <> 0;
end;

function TStackItem.Kill: Boolean;
begin
  Result := Rank < RANK_HIGH;
  if Result then
    Rank := 0;
end;

function TStackItem.NoInfo: Boolean;
begin
  Result := Flags and SIF_NO_INFO <> 0;
end;

function TStackItem.OutOfRange: Boolean;
begin
  Result := Flags and SIF_OUT_OF_RANGE <> 0;
end;

{$ENDREGION 'TStackItem'}
{$REGION 'TStackTrace'}
{ TStackTrace }

procedure TStackTrace.Clean;
begin
  FTracedItems.Clear;
end;

constructor TStackTrace.Create;
begin
  FStackInfo := Default (TStackInfo);
  FIsStackReadable := False;
  FTracedItems := TList.Create;
  FExceptionAddress := nil;
  FFilter := True;
  FTraceMethod := stCombined;
  FOptions := [soUseFirstCallOnEbp];
  FFilterOptions := [sfExcludeTryBlocks, sfUnwind, sfHideOutOfRangeCalls, sfHideUnknownCallsInfo];
end;

destructor TStackTrace.Destroy;
begin
  FTracedItems.Free;
  inherited;
end;

function TStackTrace.AcceptReturnAddress(Address: Pointer; out CallAddress, CallTargetAddress: Pointer): Boolean;
begin
  if GetCallerAddress(Address, CallAddress) then
  begin
    if GetCallTargetAddress(CallAddress, CallTargetAddress, [ctFollowJmp, ctBaseOnPrvIns]) then
      if not AcceptAddress(CallTargetAddress) then
        Exit(False);
    Exit(True);
  end;
  Result := False;
end;

function TStackTrace.AcceptReturnAddress(Address: Pointer): Boolean;
var
  Dummy1: Pointer;
  Dummy2: Pointer;
begin
  Result := AcceptReturnAddress(Address, Dummy1, Dummy2);
end;

function TStackTrace.AddressInStackRange(Address: Pointer): Boolean;
var
  LStackTop: NativeUInt;
  LStackLimit: NativeUInt;
  LAddress: NativeUInt;
begin
  LStackTop := NativeUInt(FStackInfo.StackTop);
  LStackLimit := NativeUInt(FStackInfo.StackLimit);
  LAddress := NativeUInt(Address);
  Result := (LAddress >= LStackLimit) and (LAddress < LStackTop);
end;

function TStackTrace.AcceptAddress(Address: Pointer): Boolean;
  function ModuleFromAddress: HMODULE;
  var
    mbi: TMemoryBasicInformation;
  begin
    Result := 0;
    with mbi do
    begin
      if (VirtualQuery(Address, mbi, SizeOf(TMemoryBasicInformation)) > 0) and //
        (State = MEM_COMMIT) and //
        (Protect = PAGE_EXECUTE_READ) then
        Result := HMODULE(AllocationBase);
    end;
  end;

begin
  Result := Assigned(Address) and //
    (not AddressInStackRange(Address)) and //
    (ModuleFromAddress <> 0);
end;

function TStackTrace.GetStackSize: Integer;
begin
  Result := Cardinal(NativeUInt(FStackInfo.StackTop) - NativeUInt(FStackInfo.StackLimit));
end;

function TStackTrace.GetIsStackReadable: Boolean;
begin
  try
    Result := not IsBadReadPtr(FStackInfo.StackLimit, StackSize);
  except
    Result := False;
  end;
end;

function TStackTrace.GetCount: Integer;
begin
  Result := FTracedItems.Count;
end;

function TStackTrace.GetItem(Index: Integer): PStackItem;
begin
  Result := FTracedItems[Index];
end;

procedure TStackTrace.SetStackInfo(const StackInfo: TStackInfo);
begin
  FStackInfo := StackInfo;
  FIsStackReadable := GetIsStackReadable;
end;

function TStackTrace.Trace: Boolean;
begin
  if StackSize = 0 then
    raise EStackTraceException.CreateRes(@SNullStackSize);
  if not FIsStackReadable then
    raise EStackTraceException.CreateRes(@SStackNotReadable);

  SetStackTraceError(1);
  Clean;
  Result := CustomTrace;
  ClearStackTraceError;
end;

{$ENDREGION 'TStackTrace'}
{$REGION 'TCallTrace32'}
{$IFDEF CPUX86}
{ TCallTrace32 }

procedure TCallTrace32.Clean;
var
  I: Integer;
  P: Pointer;
begin
  FCalls.Clear;
  FFrames.Clear;
  for I := 0 to FItems.Count - 1 do
  begin
    P := FItems[I];
    if Assigned(P) then
      FreeMem(P);
  end;
  FItems.Clear;
  inherited;
end;

procedure TCallTrace32.CollectEbpFrames;
var
  StackFrame: PStackFrame;
begin
  StackFrame := FStackInfo.StackFrame;
  while Assigned(StackFrame) and AddressInStackRange(StackFrame) do
  begin
    if AcceptAddress(StackFrame^.ReturnAddress) and AcceptReturnAddress(StackFrame^.ReturnAddress) then
    begin
      FFrames.Add(StackFrame);
    end;
    StackFrame := StackFrame^.CallerFrame;
  end;
end;

procedure TCallTrace32.CollectRawItems;
var
  ESP: PNativeUint;
  EBP: PNativeUint;
begin
  ESP := FStackInfo.StackPtr;
  EBP := FStackInfo.StackFrame;

  if Assigned(EBP) and (FTraceMethod = stCombined) then
  begin
    while not(AcceptAddress(PPointer(EBP)^) and AcceptReturnAddress(PPointer(EBP)^)) do
      Inc(EBP);

    if soUseFirstCallOnEbp in FOptions then
      while (ESP <> EBP) do
        Inc(ESP);
  end;

  while AddressInStackRange(ESP) do
  begin
    RegisterStackPtr(ESP);
    Inc(ESP);
  end;
end;

constructor TCallTrace32.Create;
begin
  inherited;
  FItems := TList.Create;
  FCalls := TList.Create;
  FFrames := TList.Create;
end;

procedure TCallTrace32.CollectFrameCalls;
var
  I: Integer;
  Frame: PStackFrame;
begin
  for I := 0 to FFrames.Count - 1 do
  begin
    Frame := FFrames[I];
    RegisterStackPtr(@Frame^.ReturnAddress);
  end;
end;

procedure TCallTrace32.ReRankItems(Frame: PStackFrame);
var
  I: Integer;
  PItem: PStackItem;
  LFrame: PStackFrame;
begin
  LFrame := Frame;
  while AddressInStackRange(Frame) do
  begin
    if FFrames.IndexOf(Frame) > -1 then
      Exit;
    Frame := Frame^.CallerFrame;
  end;
  while AddressInStackRange(LFrame) do
  begin
    FFrames.Add(LFrame);
    LFrame := LFrame^.CallerFrame;
  end;
  for I := 0 to FCalls.Count - 1 do
  begin
    PItem := FCalls[I];
    if (PItem^.IsValid) and IsReturnAddresInEbpFrame(PItem^.StackPtr) then
      PItem^.Rank := RANK_HIGH;
  end;
end;

function TCallTrace32.RebuildBrokenEbpChain(var NewFrame: PStackFrame): Boolean;
var
  MainFramesList: TList;
  SubFramesList: TList;

  function GetInternalFrameStruct(Frame: PStackFrame): PInternalFrameStruct;
  var
    I: Integer;
  begin
    for I := 0 to SubFramesList.Count - 1 do
    begin
      Result := SubFramesList[I];
      if Result^.Frame = Frame then
        Exit;
    end;
    Result := nil;
  end;
  procedure RegisterMainFrame(MainFrame: PStackFrame);
  begin
    if MainFramesList.IndexOf(MainFrame) = -1 then
      MainFramesList.Add(MainFrame);
  end;

  procedure RegisterSubFrames(Frame: PStackFrame);
  var
    PFrameStruct: PInternalFrameStruct;
  begin
    while ValidFrame(Frame) do
    begin
      PFrameStruct := GetInternalFrameStruct(Frame);
      if Assigned(PFrameStruct) then
        Inc(PFrameStruct^.Rank)
      else
      begin
        New(PFrameStruct);
        PFrameStruct^.Frame := Frame;
        PFrameStruct^.Rank := 0;
        SubFramesList.Add(PFrameStruct);
      end;
      Frame := Frame^.CallerFrame;
    end;
  end;

  function IsSequencedFrames(Frame: PStackFrame; Index: Integer): Boolean;
  var
    L: Integer;
    PTmp: PStackItem;
  begin
    while ValidFrame(Frame) do
    begin
      L := NativeUInt(Frame^.CallerFrame) - NativeUInt(Frame);
      L := L div SizeOf(Pointer);
      if L <= 0 then
        Exit(False);
      Inc(Index, L);
      if Index >= FItems.Count then
        Exit(False);
      PTmp := FItems[Index];
      if not PTmp^.IsValid then
        Exit(False);
      Frame := Frame^.CallerFrame;
    end;
    Result := True;
  end;

  procedure DeleteFakeFrames(StrongFrame: PStackFrame);
  var
    I: Integer;
    PFrame: PStackFrame;
  begin
    if not Assigned(StrongFrame) then
      Exit;
    for I := 0 to MainFramesList.Count - 1 do
    begin
      PFrame := MainFramesList[I];
      while ValidFrame(PFrame) do
      begin
        if PFrame = StrongFrame then
          Exit;
        PFrame := PFrame^.CallerFrame;
      end;
      MainFramesList[I] := nil;
    end;
  end;
  function GetLastEbpItemIndex: Integer;
  var
    I: Integer;
    PItem: PStackItem;
  begin
    Result := -1;
    for I := 0 to FCalls.Count - 1 do
    begin
      PItem := FCalls[I];
      if PItem^.InEbpFrames then
        Result := I;
    end;
  end;

var
  I: Integer;
  PItem: PStackItem;
  PFrame: PStackFrame;
  StrongFrame: PStackFrame;
  LastEbpItemIndex: Integer;
begin
  Result := False;
  LastEbpItemIndex := GetLastEbpItemIndex;
  if LastEbpItemIndex < 0 then
    Exit;

  MainFramesList := TList.Create;
  SubFramesList := TList.Create;

  try
    for I := LastEbpItemIndex + 1 to FCalls.Count - 1 do
    begin
      PItem := FCalls[I];
      if (not PItem^.IsValid) or (PItem^.OutOfRange) then
        Continue;
      if Assigned(PItem^.CallTargetAddress) and (not FunctionHasFrame(PItem^.CallTargetAddress)) then
        Continue;
      PFrame := Pointer(NativeUInt(PItem^.StackPtr) - SizeOf(Pointer));
      if not ValidFrame(PFrame) then
        Continue;
      if FFrames.IndexOf(PFrame) > -1 then
      begin
        { EBP frame was not broken ! }
        Exit(False);
      end;
      if IsSequencedFrames(PFrame, PItem^.iIndex) then
      begin
        RegisterMainFrame(PFrame);
        RegisterSubFrames(PFrame);
      end;
    end;
    if SubFramesList.Count > 0 then
    begin
      SubFramesList.Sort(InternalFrameListSort);
      StrongFrame := PInternalFrameStruct(SubFramesList[0])^.Frame;
      if Assigned(StrongFrame) then
        DeleteFakeFrames(StrongFrame);
      for I := 0 to MainFramesList.Count - 1 do
      begin
        NewFrame := MainFramesList[I];
        if Assigned(NewFrame) then
          Exit(True);
      end;
    end;

  finally
    for I := 0 to SubFramesList.Count - 1 do
    begin
      Dispose(SubFramesList[I]);
    end;

    MainFramesList.Free;
    SubFramesList.Free;
  end;
end;

procedure KillItemAOrItemB(A, B: PStackItem);
begin
  { We don't kill item that have the highest rank.
    Also, when they both have the same rank,
    We prefer item that have the highest call address
    because the lower one could be executed before
    calling stack trace. }
  if A^.Rank > B^.Rank then
    B^.Kill
  else if B^.Rank > A^.Rank then
    A^.Kill
  else if NativeUInt(A^.CallAddress) > NativeUInt(B^.CallAddress) then
    B^.Kill
  else if NativeUInt(B^.CallAddress) > NativeUInt(A^.CallAddress) then
    A^.Kill
  else
    A^.Kill;
end;

procedure TCallTrace32.RemoveDuplicateCalls;
var
  I: Integer;
  J: Integer;
  PItem: PStackItem;
  PTmp: PStackItem;
  nCalls: Integer;
begin
  nCalls := FCalls.Count - 1;
  for I := 0 to nCalls do
  begin
    PItem := FCalls[I];
    if (not PItem^.IsValid) or (PItem^.OutOfRange) then
      Continue;
    if (I = nCalls) then
      Break;
    for J := I + 1 to nCalls do
    begin
      PTmp := FCalls[J];
      if (not PTmp^.IsValid) or (PTmp^.OutOfRange) then
        Continue;
      if PItem^.CallAddress = PTmp^.CallAddress then
      begin
        KillItemAOrItemB(PTmp, PItem);
        Continue;
      end;
      if Assigned(PItem^.CallTargetAddress) and (PItem^.CallTargetAddress = PTmp^.CallTargetAddress) then
      begin
        KillItemAOrItemB(PTmp, PItem);
        Continue;
      end;
      if Assigned(PItem^.Info.ProcedureAddress) and (PItem^.Info.ProcedureAddress = PTmp^.Info.ProcedureAddress) then
      begin
        KillItemAOrItemB(PTmp, PItem);
        Continue;
      end;
      Break;
    end;
  end;
end;

procedure TCallTrace32.RemoveInBetweenCalls;
  function IsSafeToDelete(StartItem, EndItem: PStackItem): Boolean;
  var
    I: Integer;
    PItem: PStackItem;
    PTmp: PStackItem;
  begin
    for I := StartItem^.cIndex + 1 to EndItem^.cIndex - 1 do
    begin
      PItem := FCalls[I];
      if (not PItem^.IsValid) or (PItem^.OutOfRange) then
        Continue;
      if PItem^.Rank > RANK_HIGH then
        Exit(False);
      if Assigned(PItem^.CallTargetAddress) then
      begin
        if PItem^.CallTargetAddress = EndItem^.CallTargetAddress then
          Continue;
        PTmp := GetFirstTargetItem(PItem);
        if Assigned(PTmp) and (PTmp^.cIndex < StartItem^.cIndex) then
          Exit(False);
      end else begin
        PTmp := GetCallerItem(PItem, EndItem^.cIndex + 1, GCF_RESPECT_RANK);
        if Assigned(PTmp) then
          Exit(False);
      end;
    end;
    Result := True;
  end;

var
  I: Integer;
  J: Integer;
  PItem: PStackItem;
  PTmp: PStackItem;
  PDummy: PStackItem;
begin

  for I := FCalls.Count - 1 downto 0 do
  begin
    PItem := FCalls[I];
    if (not PItem^.IsValid) or (PItem^.OutOfRange) then
      Continue;
    PTmp := GetFirstTargetItem(PItem);
    if Assigned(PTmp) then
    begin
      if IsSafeToDelete(PTmp, PItem) then
        for J := PTmp^.cIndex + 1 to PItem^.cIndex - 1 do
        begin
          PDummy := FCalls[J];
          PDummy^.Kill;
        end;
    end;
  end;
end;

function TCallTrace32.CustomTrace: Boolean;
var
  I: Integer;
  PItem: PStackItem;
  NewFrame: PStackFrame;
  DoBuildBrokenEbpChain: Boolean;
  nCalls: Integer;
label NewTrace;
label OldTrace;
begin
  DoBuildBrokenEbpChain := True;

NewTrace: // New trace
  { It's very important to keep this order ! }

  if (FTraceMethod = stFrames) or (FTraceMethod = stCombined) then
    CollectEbpFrames;

  if (FTraceMethod = stRaw) or (FTraceMethod = stCombined) then
    CollectRawItems
  else
    CollectFrameCalls;

OldTrace: // Continue previous tracing
  if FFilter and (FTraceMethod <> stFrames) then
  begin
    if sfExcludeTryBlocks in FFilterOptions then
      RemoveTryBlocksArea;

    if (FTraceMethod = stCombined) then
    begin
      RemoveCallsBetweenTwoEbpCall;

      if sfUnwind in FFilterOptions then
      begin
        { Unwind stack only for calls
          that appear on EBP chain. }
        UnwindStack(True);
      end;
    end;

    RemoveSealedCalls;
    if sfNoTolerance in FFilterOptions then
      repeat
        nCalls := GetValidCallsCount;
        RemoveSealedCalls;
      until (nCalls = GetValidCallsCount);

    if (FTraceMethod = stCombined) then
    begin
      DispatchRank;
      if sfUnwind in FFilterOptions then
        UnwindStack(False);
    end;

    if DoBuildBrokenEbpChain and (FTraceMethod = stCombined) then
    begin
      if (soRebuildBrokenEbpChain in FOptions) then
      begin
        DoBuildBrokenEbpChain := False;
        if RebuildBrokenEbpChain(NewFrame) then
        begin
          if Assigned(NewFrame) then
            NewFrame := NewFrame^.CallerFrame;
          if ValidFrame(NewFrame) then
          begin
            if (soDropCurrentEbpChain in FOptions) then
            begin
              Clean;
              FStackInfo.StackFrame := NewFrame;
              SetStackInfo(FStackInfo);
              goto NewTrace;
            end else begin
              ReRankItems(NewFrame);
              goto OldTrace;
            end;
          end;
        end;
      end;
    end;
    RemoveInBetweenCalls;
    RemoveDuplicateCalls;
  end;

  for I := 0 to FCalls.Count - 1 do
  begin
    PItem := FCalls[I];
    if PItem^.IsValid then
    begin
      if FFilter and (sfHideOutOfRangeCalls in FFilterOptions) and (PItem^.OutOfRange) then
        Continue;
      if FFilter and (sfHideUnknownCallsInfo in FFilterOptions) and (PItem^.NoInfo) then
        Continue;
      FTracedItems.Add(PItem);
    end;
  end;
  Result := True;
end;

destructor TCallTrace32.Destroy;
begin
  Clean;
  FItems.Free;
  FCalls.Free;
  FFrames.Free;
  inherited;
end;

function TCallTrace32.GetValidLink(Item: PStackItem): PStackItem;
var
  I: Integer;
  PItem: PStackItem;
  LRank: Integer;
  PLink: PStackItem;
  PTmp: PStackItem;
begin
  { Find the best link for item !
    A link is guaranteed to be a part of EBP chain
    or a part of a call that links to EBP chain.
    Use this function instead of GetCallerItem
    only when you have an EBP frames.
    Note that this function requires an EBP chain
    to works. }

  LRank := 0;
  PLink := nil;
  Result := nil;
  if Item^.OutOfRange then
    Exit;
  for I := Item^.cIndex + 1 to FCalls.Count - 1 do
  begin
    PItem := FCalls[I];
    if (I + 1 >= FCalls.Count) then
      Break;
    if (not PItem^.IsValid) then
      Continue;
    if PItem^.CallTargetAddress = Item^.Info.ProcedureAddress then
    begin
      if PItem^.Rank >= LRank then
        PLink := PItem;
    end else begin
      if (PItem^.InEbpFrames) then
        Break;
      if (PItem^.Rank > Item^.Rank) then
      begin
        if Assigned(PLink) and (PItem^.Rank > LRank) then
          Break
        else
          Break;
      end;
      if Assigned(PLink) then
      begin
        PTmp := GetValidLink(PLink);
        if Assigned(PTmp) then
          Break;
      end;
    end;
  end;
  Result := PLink;
end;

procedure TCallTrace32.DispatchRank;
var
  I: Integer;
  PItem: PStackItem;
  PLink: PStackItem;
  LRank: Integer;
begin
  for I := 0 to FCalls.Count - 1 do
  begin
    PItem := FCalls[I];
    if not PItem^.IsValid then
      Continue;
    PLink := GetValidLink(PItem);
    if Assigned(PLink) then
    begin
      { Pass rank to linked item. }
      if PItem^.Rank > PLink^.Rank then
        LRank := PItem^.Rank
      else
        LRank := PLink^.Rank;
      if (LRank > RANK_NORMAL) then
      begin
        if (PItem^.Rank > PLink^.Rank) then
          PLink^.Rank := LRank - 1
        else if (PLink^.Rank > PItem^.Rank) then
          PItem^.Rank := LRank - 1
      end;
    end;
  end;
end;

function TCallTrace32.GetCallerItem(Item: PStackItem; Index: Integer; Flags: ShortInt): PStackItem;
var
  I: Integer;
  ProcedureAddress: Pointer;
begin
  ProcedureAddress := Item^.Info.ProcedureAddress;
  if (Index < FCalls.Count) and (not Item^.OutOfRange) and (Assigned(ProcedureAddress)) then
  begin
    for I := Index to FCalls.Count - 1 do
    begin
      Result := FCalls[I];
      if not Result^.IsValid then
        Continue;
      if (Flags and GCF_ONLY_EBP <> $00) and (not Result^.InEbpFrames) then
        Continue;
      if Result^.CallTargetAddress = ProcedureAddress then
        Exit;
      if (Flags and GCF_RESPECT_RANK <> $00) then
      begin
        if Result^.InEbpFrames then
          Break;
        if Result^.Rank > Item^.Rank then
          Break;
      end;
      if (Flags and GCF_RESPECT_ALIAS <> $00) and (Result^.Info.ProcedureAddress = ProcedureAddress) then
        Break;
    end;
  end;
  Result := nil;
end;

function TCallTrace32.GetItemFromStackPtr(StackPtr: Pointer; Index: Integer): PStackItem;
var
  I: Integer;
begin
  for I := Index to FItems.Count - 1 do
  begin
    Result := FItems[I];
    if not Result^.IsValid then
      Continue;
    if Result^.StackPtr = StackPtr then
      Exit;
  end;
  Result := nil;
end;

function TCallTrace32.GetFirstTargetItem(Item: PStackItem): PStackItem;
var
  I: Integer;
  CallTargetAddress: Pointer;
begin
  CallTargetAddress := Item^.CallTargetAddress;
  if Assigned(CallTargetAddress) then
  begin
    for I := Item^.cIndex - 1 downto 0 do
    begin
      Result := FCalls[I];
      if (not Result^.IsValid) or (Result^.OutOfRange) then
        Continue;
      if Result^.Info.ProcedureAddress = CallTargetAddress then
        Exit;
    end;
  end;
  Result := nil;
end;

function TCallTrace32.ValidFrame(Frame: PStackFrame): Boolean;
begin
  Result := Assigned(Frame) and AddressInStackRange(Frame) and AddressInStackRange(Frame^.CallerFrame);
end;

function TCallTrace32.GetValidCallsCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FCalls.Count - 1 do
  begin
    if PStackItem(FCalls[I])^.IsValid then
      Inc(Result);
  end;
end;

function TCallTrace32.GetItemEbpFrame(Item: PStackItem): PStackFrame;
var
  I: Integer;
begin
  { Always use this function to get the frame
    Don't base on stack ptr! }
  for I := 0 to FFrames.Count - 1 do
  begin
    Result := FFrames[I];
    if Assigned(Result) and (@Result^.ReturnAddress = Item^.StackPtr) then
      Exit;
  end;
  Result := nil;
end;

function TCallTrace32.RegionCouldBeDummy(AStart, AEnd: PStackItem): Boolean;
var
  I: Integer;
  TmpList: TList;
  PItem: PStackItem;
begin
  Result := False;
  TmpList := TList.Create;
  for I := AStart^.cIndex + 1 to AEnd^.cIndex - 1 do
  begin
    PItem := FCalls[I];
    if PItem^.IsValid then
    begin
      if TmpList.IndexOf(PItem^.CallAddress) > -1 then
      begin
        Result := True;
        Break;
      end;
      TmpList.Add(PItem^.CallAddress);
    end;
  end;
  TmpList.Free;
end;

function TCallTrace32.IsReturnAddresInEbpFrame(PReturnAddress: PPointer): Boolean;
var
  I: Integer;
  StackFrame: PStackFrame;
begin
  for I := 0 to FFrames.Count - 1 do
  begin
    StackFrame := FFrames[I];
    if Assigned(StackFrame) and (@StackFrame^.ReturnAddress = PReturnAddress) then
      Exit(True);
  end;
  Result := False;
end;

procedure TCallTrace32.RegisterStackPtr(StackPtr: Pointer);
var
  PItem: PStackItem;
  LReturnAddress: Pointer;
  LInfo: TAddressInfo;
begin
  GetMem(PItem, SizeOf(TStackItem));
  FillChar(PItem^, SizeOf(TStackItem), #00);
  PItem^.StackPtr := StackPtr;
  PItem^.iIndex := FItems.Add(PItem);
  PItem^.cIndex := -1;
  try
    with PItem^ do
    begin
      LReturnAddress := PPointer(StackPtr)^;
      if AcceptAddress(LReturnAddress) and AcceptReturnAddress(LReturnAddress, CallAddress, CallTargetAddress) then
      begin
        Rank := RANK_NORMAL;
        if GetAddressInfo(CallAddress, LInfo, aimAddress) then
        begin
          Info.Address := CallAddress;
          Info.ProcedureAddress := LInfo.SymbolAddress;
          Info.DebugSource := LInfo.DebugSource;
          Info.ExportIndex := LInfo.SymbolIndex;
          if not IsAddressInFunctionBody(CallAddress, Info.ProcedureAddress) then
            Flags := SIF_OUT_OF_RANGE;
        end else begin
          Flags := SIF_NO_INFO;
        end;
        if (FTraceMethod = stFrames) then
        begin
          { Avoid calling IsReturnAddresInEbpFrame function ! }
          Rank := RANK_HIGH;
        end else if IsReturnAddresInEbpFrame(StackPtr) then
          Rank := RANK_HIGH;
        cIndex := FCalls.Add(PItem);
      end;
    end;
  except
    // Continue.
  end;
end;

procedure TCallTrace32.RemoveCallsBetweenTwoEbpCall;
var
  I: Integer;
  J: Integer;
  PItem: PStackItem;
  PCaller: PStackItem;
  PDummy: PStackItem;
  PFrame: PStackFrame;
  L: Integer;
begin
  { Remove all calls between two ebp call. }
  for I := 0 to FCalls.Count - 1 do
  begin
    PItem := FCalls[I];
    if not PItem^.InEbpFrames then
      Continue;
    { Here's the trick, we will try to make a link
      between a call (that has an ebp frame) and it's caller.
      If the caller target address is known .. that's good !
      There is a direct link between those two call.
      If not we try to check if the caller has an ebp frame,
      if so there is an indirect link between those two call.
      Whatever was the link type (direct or indirect)
      we delete all calls in between.
      Otherwise,if there is no link, we don't delete. }
    L := 0;
    PCaller := GetCallerItem(PItem, I + 1, GCF_ONLY_EBP or GCF_RESPECT_RANK);
    if Assigned(PCaller) then
    begin
      L := PCaller^.iIndex;
    end else if ((not PItem^.OutOfRange) and FunctionHasFrame(PItem^.Info.ProcedureAddress)) then
    begin
      PFrame := GetItemEbpFrame(PItem);
      if AddressInStackRange(PFrame^.CallerFrame) then
      begin
        { Must check ... It could be the last ebp frame ! }
        L := NativeUInt(PFrame^.CallerFrame) - NativeUInt(PItem^.StackPtr) - SizeOf(Pointer);
        L := L div SizeOf(Pointer);
        L := PItem^.iIndex + L;
      end;
    end;
    if L > 0 then
    begin
      for J := PItem^.iIndex + 1 to L - 1 do
      begin
        if J >= FItems.Count then
          Break;
        PDummy := FItems[J];
        PDummy^.Kill;
      end;
    end;
  end;
end;

procedure TCallTrace32.RemoveSealedCalls;
  function CouldBeStdCall(FunctionStartAddress, CallAddress: Pointer): Boolean;
  var
    Ins: TInstruction;
    P: Pointer;
    nPush: Integer;
    n: Integer;
    LastPush: Integer;
  begin
    { Hard way to check if a function's calling convention.
      When the call's target addess is unknown, this function
      will do the trick !
      What about getting it's params size ?
      Well, that's too risky !
      Compiler may push few arguments then calls a no stdcall
      function, then push the rest of arguments and finally,
      it calls the stdcall function. }
    P := FunctionStartAddress;
    if not Assigned(P) then
      Exit(True); // ?
    nPush := 0;
    n := 0;
    LastPush := 0;
    while True do
    begin
      if IsBadCodePtr(P) then
        Break;
      Inc(n);
      Ins := TInstruction.Create;
      Ins.Arch := CPUX;
      Ins.Syntax := SX_NIL_SYNTAX;
      Ins.Addr := P;
      Disasm(@Ins);
      Ins.Free;
      case Ins.InstID of
        INST_ID_PUSH:
          begin
            LastPush := n;
            Inc(nPush);
          end;
        INST_ID_JB .. INST_ID_JZ:
          begin
            nPush := 0;
          end;
        INST_ID_CALL:
          begin
            if n - LastPush > 3 then
              nPush := 0;
            if P = CallAddress then
            begin
              Exit(nPush > 0);
            end;
            nPush := 0;
          end;
        INST_ID_RET:
          begin
            Break;
          end;
      end;
      P := Ins.NextInst;
    end;
    Result := False;
  end;

  function GetStackOffset(PItem: PStackItem; StackFrame: PStackFrame; var ParamsSize: Integer; var NeedCallParamsSize: Boolean): Integer;
  var
    P: Pointer;
    Ins: TInstruction;
    n: Integer;
    DefaultSize: Integer;
  begin
    NeedCallParamsSize := True;
    P := PPointer(PItem^.StackPtr)^; // Return address.
    n := 0;
    Result := 0;
    while True do
    begin
      if IsBadCodePtr(P) then
        Break;
      Ins := TInstruction.Create;
      Ins.Arch := CPUX;
      Ins.Syntax := SX_NIL_SYNTAX;
      Ins.Addr := P;
      Disasm(@Ins);
      Ins.Free;
      if Ins.Prefixes.OpSizePrf.Flags and (PF_USED or PF_VALID) = (PF_USED or PF_VALID) then
        DefaultSize := SizeOf(Word)
      else
        DefaultSize := SizeOf(Pointer);
      P := Ins.NextInst;
      case Ins.InstID of
        INST_ID_CALL:
          begin
            { Some calls such DoneExcept may change
              esp register ... so we exit if our call
              is not the last call ! }
            Exit(0);
          end;
        INST_ID_JB .. INST_ID_JLE, INST_ID_JNB .. INST_ID_JZ:
          begin
            { That's too bad, we can't know if wether we follow
              this jmp or not !
              It's too risky to follow if it's was not executed.
              And it's the same if we don't follow and it was executed.
              In the two situation, we may have wrong result.
              So we leave ! }
            Exit(0);
          end;
        INST_ID_JMP:
          begin
            if Assigned(Ins.DstAddr.Addr) then
              P := Ins.DstAddr.Addr
            else
              Exit(0);
          end;
        INST_ID_NOP:
          begin
            Continue;
          end;
        { Emulate the stack }
        INST_ID_MOV:
          begin
            { We can't emulate if it's not mov esp,ebp ! }
            if (Ins.Arg1.Reg = REG_ESP) then
            begin
              if (Ins.Arg2.Reg = REG_EBP) then
              begin
                { mov esp,ebp }
                if (not Assigned(StackFrame)) or (not AddressInStackRange(StackFrame^.CallerFrame)) then
                  Exit(0);
                n := NativeUInt(StackFrame^.CallerFrame) - 4 - NativeUInt(PItem^.StackPtr);
                NeedCallParamsSize := False;
              end
              else
                Exit(0);
            end
          end;
        INST_ID_POP:
          begin
            Inc(n, DefaultSize);
          end;
        INST_ID_PUSH:
          begin
            if Ins.Arg1.Reg <> 0 then
            begin
              { Don't take the risk !
                push reg
                ...
                ret
                ---------------------------------
                It could be an indirect call !!! }
              Exit(0);
            end;
            if (Ins.Arg1.Flags and AF_TYPE_MASK = AF_IMM) and (AcceptAddress(Pointer(Ins.Arg1.Imm.Value))) then
            begin
              { push address
                ...
                ret
                ------------------------------
                This is an indirect call !!! }
              Exit(0);
            end;
            Dec(n, DefaultSize);
          end;
        INST_ID_PUSHF:
          begin
            Dec(n, DefaultSize);
          end;
        INST_ID_PUSHAD:
          begin
            Dec(n, DefaultSize * 8);
          end;
        INST_ID_POPF:
          begin
            Inc(n, DefaultSize);
          end;
        INST_ID_POPAD:
          begin
            Inc(n, DefaultSize * 8);
          end;
        INST_ID_ADD:
          begin
            if (Ins.Arg1.Reg = REG_ESP) and (Ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) then
            begin
              n := n + Integer(Ins.Arg2.Imm.Value);
            end;
          end;
        INST_ID_SUB:
          begin
            if (Ins.Arg1.Reg = REG_ESP) and (Ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM) then
            begin
              n := n - Integer(Ins.Arg2.Imm.Value);
            end;
          end;
        INST_ID_RET:
          begin
            { Pop function's params }
            ParamsSize := Ins.Arg1.Imm.Value;
            Inc(n, ParamsSize);
            { Pop return address }
            Inc(n, SizeOf(Pointer));
            Exit(n);
          end;
      end;
    end;
  end;

  function ValidSubCalls(Address, MaxAddress: Pointer; Index: Integer): Boolean;
    function ReturnAddressExists(Address: Pointer; Index: Integer): Boolean;
    var
      I: Integer;
      PItem: PStackItem;
    begin
      for I := Index downto 0 do
      begin
        PItem := FCalls[I];
        if PItem^.IsValid and (PPointer(PItem^.StackPtr)^ = Address) then
          Exit(True);
      end;
      Result := False;
    end;

    function TargetExists(Address: Pointer; Index: Integer): Boolean;
    var
      I: Integer;
      PItem: PStackItem;
    begin
      for I := Index downto 0 do
      begin
        PItem := FCalls[I];
        if (PItem^.IsValid) and (not PItem^.OutOfRange) and (Assigned(PItem^.Info.ProcedureAddress)) and (PPointer(PItem^.Info.ProcedureAddress)^ = Address) then
          Exit(True);
      end;
      Result := False;
    end;

  var
    Ins: TInstruction;
    P: Pointer;
    Jmp: Pointer;
    Target: Pointer;
    FollowJmp: Boolean;
    IsDynaMethod: Boolean;
  begin
    P := Address;
    if not Assigned(P) then
      Exit(True); // We don't know !

    if Assigned(MaxAddress) then
      Assert(NativeUInt(MaxAddress) > NativeUInt(Address));

    Jmp := nil;
    FollowJmp := True;
    IsDynaMethod := False;
    while True do
    begin
      if Assigned(MaxAddress) and (NativeUInt(P) >= NativeUInt(MaxAddress)) then
        Break;
      if IsBadCodePtr(P) then
        Break;
      Ins := TInstruction.Create;
      Ins.Arch := CPUX;
      Ins.Syntax := SX_NIL_SYNTAX;
      Ins.Addr := P;
      Disasm(@Ins);
      Ins.Free;
      P := Ins.NextInst;
      case Ins.InstID of
        INST_ID_JMP:
          begin
            Target := Ins.DstAddr.Addr;
            if not Assigned(Target) then
            begin
              if IsDynaMethod then
              begin
                { Dynamic methods use jmp to jump
                  to the target address. If we found
                  a call to System.GetDynaMethod function
                  we exit (True). }
                Exit(True);
              end;
            end else begin
              if TargetExists(Target, Index - 1) then
                Exit(True);
              { Do we need to follow this jump ? }
              if FollowJmp then
              begin
                { Yep, it's safe, it was no conditional jump
                  before this instruction. }
                P := Target;
              end;
            end;

          end;
        INST_ID_JB .. INST_ID_JLE, INST_ID_JNB .. INST_ID_JZ:
          begin
            { conditional jumps }
            FollowJmp := False;
            Target := Ins.DstAddr.Addr;
            { We collect only the highest target address.
              We will use it to check if a ret instruction
              is the last instruction in that function or not. }
            if NativeUInt(Target) > NativeUInt(Jmp) then
              Jmp := Target;
          end;
        INST_ID_CALL:
          begin
            FollowJmp := False;
            if ReturnAddressExists(P, index - 1) then
              Exit(True);
            Target := Ins.DstAddr.Addr;
            if Assigned(Target) then
              IsDynaMethod := IsGetDynaMethodCall(Target);
          end;
        INST_ID_RET:
          begin
            if not Assigned(MaxAddress) then
            begin
              { Check if it's not a (case/[if .. then] or loop) statement
                If so break ... it's the end !
                If not continue, may be there is some calls out there ! }
              if NativeUInt(P) > NativeUInt(Jmp) then
              begin
                Break;
              end;
            end;
          end;
      end;
    end;
    Result := False;
  end;

var
  I: Integer;
  PItem: PStackItem;
  offset: Integer;
  CallParamsSize: Integer;
  ParamsSize: Integer;
  PTmp: PStackItem;
  P: Pointer;
  J: Integer;
  PFrame: PStackFrame;
  NeedParams: Boolean;
  Strong: Boolean;
  Max: Pointer;
begin
  { What is a sealed call ?
    I define a sealed call as a valid call ... But this call does not make part of traced calls.
    It could be an empty call or a call that was run before entering the trace. }

  for I := 0 to FCalls.Count - 1 do
  begin
    PItem := FCalls[I];
    if not PItem^.IsValid then
      Continue;

    if IsHaltCall(PPointer(PItem^.StackPtr)^) then
      Continue;

    Strong := PItem^.InEbpFrames;

    CallParamsSize := 0;
    ParamsSize := 0;

    P := PItem^.CallTargetAddress;
    if Assigned(P) then
    begin
      if FunctionHasFrame(P) then
      begin
        PFrame := Pointer(NativeUInt(PItem^.StackPtr) - SizeOf(Pointer));
        if ValidFrame(PFrame) then
        begin
          J := NativeUInt(PFrame^.CallerFrame) - NativeUInt(PFrame);
          J := J div SizeOf(Pointer);
          J := PItem^.iIndex + J;
          if J < FItems.Count then
          begin
            PTmp := FItems[J];
            if not PTmp^.IsValid then
            begin
              PItem^.Kill;
              Continue;
            end;
          end;
        end else begin
          { Not a valid frame ? ... but wait ... Some function such
            Application.ProcessMessages use ebp register for another
            purpose. Fortunately, each valid function must restore
            the stack pointer and base pointer register to allow the
            caller to work correctly. And it happens that the caller
            pushes the ebp register before calling the target function.
            So, ebp value should be found on : return address + 4 byte. }

          PFrame := Pointer(NativeUInt(PItem^.StackPtr) + SizeOf(Pointer));
          if not ValidFrame(PFrame) then
          begin
            PItem^.Kill;
            Continue;
          end;
        end;
      end;

      if I > 0 then
      begin
        Max := GetNextSymbolAddress(P);
        { Check if sub calls are valid.
          At least one of them, its return
          address exists. }
        if not ValidSubCalls(P, Max, PItem^.cIndex) then
        begin
          PItem^.Kill;
          Continue;
        end;
      end;

      CallParamsSize := GetFunctionParamsSize(P)
    end else begin
      if PItem^.OutOfRange then
        CallParamsSize := -1
      else if CouldBeStdCall(PItem^.Info.ProcedureAddress, PItem^.CallAddress) then
        CallParamsSize := -1;
    end;

    PFrame := GetItemEbpFrame(PItem);

    offset := GetStackOffset(PItem, PFrame, ParamsSize, NeedParams);
    offset := offset - ParamsSize;
    offset := offset div SizeOf(Pointer);
    if (offset > 0) and (NeedParams) and (CallParamsSize = -1) then
      offset := 0;
    if (offset > 0) and (CallParamsSize > -1) then
    begin
      { If we are here, that means our call is the last
        one and the stack was emulated correctly.
        That's something nice ... Try to emulate
        how the CPU works and check when returning, we return
        to a valid call. If so that call was not fake. Otherwise, if we
        return to a fake call or to something that was not a call
        we eliminate this item. }

      CallParamsSize := CallParamsSize div SizeOf(Pointer);
      offset := offset + CallParamsSize;
      offset := offset + PItem^.iIndex;
      if offset < FItems.Count then
      begin
        PTmp := FItems[offset];
        if not PTmp^.IsValid then
        begin
          PItem^.Kill;
        end else begin
          if (not(PTmp^.cIndex > PItem^.cIndex)) then
          begin
            { Also respect the order !
              When returning, it must return to the next call
              and not to a previous call ! }
            PItem^.Kill;
          end;
        end;
        if not PItem^.IsValid then
          Continue;

        { If one is in ebp chain ... we remove all between calls. }
        if PTmp^.InEbpFrames then
          Strong := True;
        if Strong or RegionCouldBeDummy(PItem, PTmp) then
        begin
          for J := PItem^.iIndex + 1 to offset - 1 do
          begin
            PTmp := FItems[J];
            PTmp^.Kill;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCallTrace32.RemoveTryBlocksArea;
var
  PFrame: PExceptionFrame;
  PItem: PStackItem;
  Index: Integer;
begin
  { Remove garbage data related to try blocks from the stack }
  PFrame := FStackInfo.ExceptionFrame;
  try
    while Assigned(PFrame) and AddressInStackRange(PFrame) do
    begin
      PItem := GetItemFromStackPtr(PFrame, 0);
      if Assigned(PItem) then
      begin
        Index := PItem^.iIndex;
        PStackItem(FItems[Index + 0])^.Kill;
        PStackItem(FItems[Index + 1])^.Kill;
        PStackItem(FItems[Index + 2])^.Kill;
      end;
      PFrame := PFrame^.Next;
    end;
  except
    // Continue.
  end;
end;

procedure TCallTrace32.UnwindStack(OnlyEbp: Boolean);
  function GetUnwindCodeSize(Address: Pointer): Integer;
  var
    Ins: TInstruction;
    P: Pointer;
    n: Integer;
    Hint: Integer;
  begin
    { Unwind the stack and return the size of garbage data.
      A negative result means that the stack contains garbage
      data.
      If we don't unwind wisely, we will have wrong result
      and we will delete calls that could be valid.
      So the basic idea is to find this sequence :
      push ebp
      mov ebp,esp
      add/sub esp,$xxxx

      We ignore all the rest because the program may restore
      the stack (pop for example) before returning to the caller.
      Usually, for the previous sequence,
      the compiler will restore the stack before the ret instruction.
      That means they are safe to catch ! }

    Result := 0;
    P := Address;
    n := 0;
    Hint := 0;
    while True do
    begin
      if IsBadCodePtr(P) then
        Break;
      Ins := TInstruction.Create;
      Ins.Arch := CPUX;
      Ins.Syntax := SX_NIL_SYNTAX;
      Ins.Addr := P;
      Disasm(@Ins);
      Ins.Free;
      P := Ins.NextInst;
      case Ins.InstID of
        INST_ID_PUSH:
          begin
            if Ins.Arg1.Reg = REG_EBP then
            begin
              { push ebp }
              Dec(n, SizeOf(Pointer));
              Hint := 1;
              Continue;
            end
            else
              Break;
          end;
        INST_ID_AND:
          begin
            { and esp,$xxxx }
            if Ins.Arg1.Reg = REG_ESP then
              Continue;
          end;
        INST_ID_MOV:
          begin
            if Ins.Arg1.Reg = REG_EBP then
            begin
              if Ins.Arg2.Reg = REG_ESP then
              begin
                { mov ebp,esp }
                if Hint <> 1 then
                  Break;
                Hint := 2;
                Continue;
              end
              else
                Break;
            end;
          end;
        INST_ID_ADD:
          begin
            { add esp,$xxxx }
            if Ins.Arg1.Reg = REG_ESP then
            begin
              if Ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM then
              begin
                if Hint <> 2 then
                  Break;
                n := n + Integer(Ins.Arg2.Imm.Value);
                Exit(n);
              end
              else
                Break;
            end;
          end;
        INST_ID_SUB:
          begin
            { sub esp,$xxxx }
            if Ins.Arg1.Reg = REG_ESP then
            begin
              if Ins.Arg2.Flags and AF_TYPE_MASK = AF_IMM then
              begin
                if Hint <> 2 then
                  Break;
                n := n - Integer(Ins.Arg2.Imm.Value);
                Exit(n);
              end
              else
                Break;
            end;
          end;
        INST_ID_CALL, INST_ID_RET, INST_ID_JMP:
          begin
            Break;
          end;
        INST_ID_NOP:
          begin
            Continue;
          end;
      end;
      Hint := 0;
    end;
  end;

  function KillItems(PItem: PStackItem; Index, Count: Integer): Boolean;
  var
    TmpList: TList;
    I: Integer;
    PTmp: PStackItem;
    EndIndex: Integer;
  begin
    Result := True;
    TmpList := TList.Create;
    EndIndex := Index + Count;
    for I := Index to EndIndex do
    begin
      if not((I >= 0) and (I < FItems.Count)) then
        Break;
      PTmp := FItems[I];
      TmpList.Add(PTmp);
      if PTmp^.Rank > PItem^.Rank then
      begin
        TmpList.Clear;
        Result := False;
        Break;
      end;
    end;
    if Result then
    begin
      for I := 0 to TmpList.Count - 1 do
      begin
        PTmp := TmpList[I];
        PTmp^.Kill;
      end;
    end;
    TmpList.Free;
  end;

var //
  I: Integer;
  PItem: PStackItem;
  ParamsSize: Integer;
  LocalsSize: Integer;
  P: Pointer;
begin

  for I := 0 to FCalls.Count - 1 do
  begin
    PItem := FCalls[I];
    if not PItem^.IsValid then
      Continue;
    if OnlyEbp and (not PItem^.InEbpFrames) then
      Continue;
    if PItem^.Rank <= RANK_NORMAL then
      Continue;
    if PItem^.OutOfRange and (not PItem^.InEbpFrames) then
      Continue;

    ParamsSize := 0;
    P := PItem^.CallTargetAddress;
    if Assigned(P) then
    begin
      { push param 1
        push param 2
        ...
        push param n
        push return address
        call @target
        return address }

      ParamsSize := GetFunctionParamsSize(P);
      if ParamsSize < 0 then
        Continue;

      ParamsSize := ParamsSize div SizeOf(Pointer);

      if (ParamsSize > 0) and (not KillItems(PItem, PItem^.iIndex + 1, ParamsSize)) then
        PItem^.Kill;

      if not PItem^.IsValid then
        Continue;

      { Now follow the target address and get it's locals data size. }
      LocalsSize := GetUnwindCodeSize(P);
      LocalsSize := LocalsSize div SizeOf(Pointer);
      if LocalsSize < 0 then
      begin
        LocalsSize := -LocalsSize;
        { push param 1
          push param 2
          ...
          push param n
          push return address
          call @target
          return address
          ...
          @target:
          push ebp
          mov ebp,esp
          add esp,$xxxx }

        if not KillItems(PItem, PItem^.iIndex - 1 - LocalsSize, LocalsSize) then
          PItem^.Kill;
      end;
    end;

    { Here, a call must have a valid ProcedureAddress ! }
    if (not PItem^.IsValid) or (PItem^.OutOfRange) then
      Continue;

    P := PItem^.Info.ProcedureAddress;
    { Remove locals from caller function. }
    LocalsSize := GetUnwindCodeSize(P);
    LocalsSize := LocalsSize div SizeOf(Pointer);
    if LocalsSize < 0 then
    begin
      { I used this trick to eliminate unit initialization procedure.
        If a call to halt found that means that the next call
        is not lincked to this call (thre is no ret instruction).
        Thus if we try to unwind this call we will have a wrong
        result because there is no ret instruction after initialization procedure. }
      if IsHaltCall(PPointer(PItem^.StackPtr)^) then
        Continue;
      LocalsSize := -LocalsSize;
      if not KillItems(PItem, PItem^.iIndex + 1 + ParamsSize { Skip call params } , LocalsSize) then
        PItem^.Kill;
    end;
  end;
end;
{$ENDIF CPUX86}
{$ENDREGION 'TCallTrace32'}
{$REGION 'TCallTrace64'}
{$IFDEF CPUX64}
{ TCallTrace64 }

constructor TCallTrace64.Create;
begin
  inherited;
  FCalls := TList.Create;
end;

destructor TCallTrace64.Destroy;
begin
  Clean;
  FCalls.Free;
  inherited;
end;

procedure TCallTrace64.AddNewItem(ReturnAddress: Pointer);
var
  PItem: PStackItem;
  LInfo: TAddressInfo;
begin
  GetMem(PItem, SizeOf(TStackItem));
  FillChar(PItem^, SizeOf(TStackItem), #00);
  PItem^.ReturnAddress := ReturnAddress;
  if not AcceptReturnAddress(ReturnAddress, PItem^.CallAddress, PItem^.CallTargetAddress) then
  begin
    FreeMem(PItem);
    Exit;
  end;
  if GetAddressInfo(ReturnAddress, LInfo, aimAddress) then
  begin
    PItem^.Info.Address := ReturnAddress;
    PItem^.Info.ProcedureAddress := LInfo.SymbolAddress;
    PItem^.Info.DebugSource := LInfo.DebugSource;
  end else begin
    if FFilter and (sfHideUnknownCallsInfo in FFilterOptions) then
    begin
      FreeMem(PItem);
      Exit;
    end;
  end;
  if Assigned(PItem^.Info.ProcedureAddress) and (not IsAddressInFunctionBody(ReturnAddress, PItem^.Info.ProcedureAddress)) then
  begin
    PItem^.Flags := SIF_OUT_OF_RANGE;
    if FFilter and (sfHideOutOfRangeCalls in FFilterOptions) then
    begin
      FreeMem(PItem);
      Exit;
    end;
  end;
  PItem^.Rank := RANK_NORMAL;
  FCalls.Add(PItem);
end;

procedure TCallTrace64.Clean;
var
  I: Integer;
  P: Pointer;
begin
  for I := 0 to FCalls.Count - 1 do
  begin
    P := FCalls[I];
    if Assigned(P) then
      FreeMem(P);
  end;
  FCalls.Clear;
  inherited;
end;

function TCallTrace64.CustomTrace: Boolean;
  function CaptureStackBackTrace(FramesToSkip: ULONG; FramesToCapture: ULONG; BackTrace: PVOID; var BackTraceHash: ULONG): USHORT; stdcall;

  type
    TCaptureStackBackTrace = function(FramesToSkip: ULONG; FramesToCapture: ULONG; BackTrace: PVOID; var BackTraceHash: ULONG): USHORT; stdcall;

  var
    FreeLib: Boolean;
    KernelModule: HMODULE;
    CaptureStackBackTraceProc: TCaptureStackBackTrace;

  begin
    KernelModule := GetModuleHandle(kernel32);
    FreeLib := KernelModule = 0;
    if FreeLib then
      KernelModule := LoadLibrary(kernel32);

    @CaptureStackBackTraceProc := GetProcAddress(KernelModule, 'RtlCaptureStackBackTrace');
    Result := CaptureStackBackTraceProc(FramesToSkip, FramesToCapture, BackTrace, BackTraceHash);

    if FreeLib and (KernelModule <> 0) then
      FreeLibrary(KernelModule);
  end;

var
  BackTrace: array [0 .. High(ShortInt)] of Pointer;
  FramesToCapture: Integer;
  CapturedFramesCount: Integer;
  Hash: DWORD;
  I: Integer;
  P: Pointer;
  PItem: PStackItem;
begin
  Clean;
  if CheckWin32Version(6, 0) then
    FramesToCapture := High(ShortInt)
  else
    FramesToCapture := 62; // Win XP.
  ZeroMemory(@BackTrace[0], SizeOf(BackTrace));
  CapturedFramesCount := CaptureStackBackTrace(1, FramesToCapture, @BackTrace, Hash);

  Result := CapturedFramesCount > 0;
  if not Result then
    Exit;

  for I := 1 to CapturedFramesCount - 1 do
  begin
    P := BackTrace[I];
    AddNewItem(P);
  end;

  for I := 0 to FCalls.Count - 1 do
  begin
    PItem := FCalls[I];
    if PItem^.IsValid then
    begin
      FTracedItems.Add(PItem);
    end;
  end;
end;

{$ENDIF CPUX64}
{$ENDREGION 'TCallTrace64'}
{$REGION 'TTryTrace'}
{$IFDEF STACK_BASED_EXCEPTIONS}
{ TTryTrace }

procedure TTryTrace.RegisterFrame(Frame: PExceptionFrame);
var
  PItem: PStackItem;
  LInfo: TAddressInfo;
  Handler: Pointer;
  P: Pointer;
begin
  try
    P := Frame^.Target;
    GetMem(PItem, SizeOf(TStackItem));
    FillChar(PItem^, SizeOf(TStackItem), #00);
    GetAddressInfo(P, LInfo, aimAddress);

    GetCallTargetAddress(P, Handler, []);
    with PItem^ do
    begin
      Info.Address := P;
      Info.ProcedureAddress := LInfo.SymbolAddress;
      Info.DebugSource := LInfo.DebugSource;
      Rank := RANK_NORMAL;
      ExceptionFrame := Frame;
      ExceptionHandler := Handler;
      with FHandlersPtr do
      begin
        if Handler = HandleFinallyPtr then
          TryType := ttFinally
        else if ((Handler = HandleAnyExceptionPtr) or (Handler = HandleOnExceptionPtr) or (Handler = HandleAutoException)) then
          TryType := ttExcept
        else
          TryType := ttUnknown;
      end;
    end;

    FTracedItems.Add(PItem);
  except

  end;
end;

procedure TTryTrace.Clean;
var
  I: Integer;
  P: Pointer;
begin
  for I := 0 to FTracedItems.Count - 1 do
  begin
    P := FTracedItems[I];
    if Assigned(P) then
      FreeMem(P);
  end;
  inherited;
end;

constructor TTryTrace.Create;
begin
  inherited;
  GetDelphiExceptionHandlersPtr(FHandlersPtr);
end;

function TTryTrace.CustomTrace: Boolean;
var
  PFrame: PExceptionFrame;
begin
  Clean;
  PFrame := FStackInfo.ExceptionFrame;
  try
    while Assigned(PFrame) and AddressInStackRange(PFrame) do
    begin
      if AcceptAddress(PFrame^.Target) then
        RegisterFrame(PFrame);
      PFrame := PFrame^.Next;
    end;
  except
  end;
  Result := True;
end;

destructor TTryTrace.Destroy;
begin
  Clean;
  inherited;
end;
{$ENDIF STACK_BASED_EXCEPTIONS}
{$ENDREGION 'TTryTrace'}

initialization

ReportMemoryLeaksOnShutdown := True;

end.
