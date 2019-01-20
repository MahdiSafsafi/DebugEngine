// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.Core
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.Core.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

unit DebugEngine.Core;

interface

uses
  WinApi.Windows;
{$I DebugEngine.inc}
{ .$DEFINE DEVMODE }

type

  TTryType = (ttUnknown, ttFinally, ttExcept);

  TTryBlockInfo = record
    /// <summary> The start address of a function that implemented try instruction.
    /// </summary>
    /// <remarks> Pass this address to <see cref="GetAddressInfo"/> function to get it's name.
    /// </remarks>
    FunctionStartAddress: Pointer;
    /// <summary> The end address of a function that implemented try instruction.
    /// </summary>
    FunctionEndAddress: Pointer;
    /// <summary> Start address of try instruction.
    /// </summary>
    TryStartAddress: Pointer;
    /// <summary> End address of try instruction.
    /// <para></para>
    /// This could be <c>finally</c> or <c>except</c> instruction depending on TryType field.
    /// </summary>
    TryEndAddress: Pointer;
    /// <summary> Address to the first instruction that will be executed if exception occurs.
    /// </summary>
    TryTargetAddress: Pointer;
    /// <summary> Try block type.
    /// <para></para>
    /// <c>ttFinally</c>: Try .. Finally block.
    /// <para></para>
    /// <c>ttExcept</c>: Try .. Except block.
    /// </summary>
    TryType: TTryType;
    /// <summary> EnumTryBlocks flags.
    /// </summary>
    /// <remarks>
    /// Flags can be only 0 or 1.
    /// <para></para>
    /// <c>1</c> Indicate that EnumTryBlocks started to process new function.
    /// And all the try blocks follow will belong to this function till
    /// a new Flags that will be set to 1.
    /// </remarks>
    ///
    Flags: Integer;
  end;

  PTryBlockInfo = ^TTryBlockInfo;

  THandlersPtr = record
    { Only for x64. }
    SEHandler: Pointer;
    { Only for x86-32. }
    HandleFinallyPtr: Pointer;
    HandleAnyExceptionPtr: Pointer;
    HandleOnExceptionPtr: Pointer;
    HandleAutoException: Pointer;
  end;

  PHandlersPtr = ^THandlersPtr;

  TStackInfo = record
    StackPtr: Pointer; // ESP or RSP.
    StackFrame: Pointer; // EBP or RBP. See DebugEngine.Trace.PStackFrame.
    StackTop: Pointer; // Top of stack.
    StackLimit: Pointer; // Stack limit
{$IFDEF STACK_BASED_EXCEPTIONS}
    ExceptionFrame: Pointer; // See DebugEngine.Trace.PExceptionFrame.
{$ENDIF STACK_BASED_EXCEPTIONS}
  end;

  PStackInfo = ^TStackInfo;

  TEnumTryCallBack = function(var Info: TTryBlockInfo;
    UserData: Pointer): Boolean;

  /// <summary> Get current thread stack info.
  /// </summary>
procedure GetStackInfo(var Info: TStackInfo);

/// <summary> Get default exception handlers pointer.
/// </summary>
procedure GetDelphiExceptionHandlersPtr(var Info: THandlersPtr);

/// <summary> Enum all try blocks related to the specified module.
/// </summary>
/// <param name="ModuleHandle"> A handle to the module. If this parameter is NULL(0), this function will use the calling module handle.
/// </param>
/// <param name="CallBackFunction"> A pointer to <see cref="TEnumTryCallBack"/> callback function. This callback function will be called
/// by EnumTryBlocks function each time the function detectes a try block. If the callback returns False, the function breaks and returns.
/// </param>
/// <param name="UserData"> Optional data to pass to the callback function.
/// </param>
/// <returns> If the function succeeds, the return value is True.
/// </returns>
/// <remarks> The functions works only if the platform is x64.
/// <para></para>
/// On x86-32, the function will return False.
/// </remarks>
function EnumTryBlocks(ModuleHandle: HMODULE;
  CallBackFunction: TEnumTryCallBack; UserData: Pointer): Boolean;

implementation

uses
  DebugEngine.PeUtils;

{$REGION 'ASM'}
{ Disable stack frames for all asm code. }
{$STACKFRAMES OFF}

procedure GetStackInfo(var Info: TStackInfo);
asm
  {$IFDEF CPU64BITS}
  mov   [rcx].TStackInfo.StackPtr,rsp
  mov   [rcx].TStackInfo.StackFrame,rbp
  mov         rax, gs:[ABS $08]
  mov   [rcx].TStackInfo.StackTop, rax
  mov         rax, gs:[ABS $10]
  mov   [rcx].TStackInfo.StackLimit,rax
  {$ELSE !CPU64BITS}
  mov   [eax].TStackInfo.StackPtr,esp
  mov   [eax].TStackInfo.StackFrame,ebp
  push        ecx
  mov         ecx,fs:[$04]
  mov   [eax].TStackInfo.StackTop, ecx
  mov         ecx,fs:[$08]
  mov   [eax].TStackInfo.StackLimit,ecx
  {$IFDEF STACK_BASED_EXCEPTIONS}
  xor         ecx,ecx
  mov         ecx,fs:[ecx]
  mov   [eax].TStackInfo.ExceptionFrame,ecx
  {$ENDIF STACK_BASED_EXCEPTIONS}
  pop         ecx
  {$ENDIF CPU64BITS}
end;

procedure GetDelphiExceptionHandlersPtr(var Info: THandlersPtr);
asm
  {$IFDEF STACK_BASED_EXCEPTIONS }
  mov dword  [eax].THandlersPtr.HandleFinallyPtr,        offset System.@HandleFinally
  mov dword  [eax].THandlersPtr.HandleAnyExceptionPtr,   offset System.@HandleAnyException
  mov dword  [eax].THandlersPtr.HandleOnExceptionPtr,    offset System.@HandleOnException
  mov dword  [eax].THandlersPtr.HandleAutoException,     offset System.@HandleAutoException
  mov dword  [eax].THandlersPtr.SEHandler,               $00000000
  {$ELSE STACK_BASED_EXCEPTIONS }
  mov  rax,  offset System.@DelphiExceptionHandler
  mov  qword [rcx].THandlersPtr.SEHandler ,              rax
  { Fill all the rest with nil }
  xor rax,rax
  mov qword  [rcx].THandlersPtr.HandleFinallyPtr,        rax
  mov qword  [rcx].THandlersPtr.HandleAnyExceptionPtr,   rax
  mov qword  [rcx].THandlersPtr.HandleOnExceptionPtr,    rax
  mov qword  [rcx].THandlersPtr.HandleAutoException,     rax
  {$ENDIF STACK_BASED_EXCEPTIONS}
end;

{ Enable stack frames for all pascal code. }
{$STACKFRAMES ON}
{$ENDREGION 'ASM'}
{$REGION 'SEH'}
{ Structured exception handling }
{$IFDEF TABLE_BASED_EXCEPTIONS}
{ =====> Windows struct (SDK) <===== }
const
  UNW_FLAG_NHANDLER = 0;
{$EXTERNALSYM UNW_FLAG_NHANDLER}
  UNW_FLAG_EHANDLER = 1;
{$EXTERNALSYM UNW_FLAG_EHANDLER}
  UNW_FLAG_UHANDLER = 2;
{$EXTERNALSYM UNW_FLAG_UHANDLER}
  UNW_FLAG_CHAININFO = 4;
{$EXTERNALSYM UNW_FLAG_CHAININFO}

type

  _RUNTIME_FUNCTION = packed record
    (*
      typedef struct _RUNTIME_FUNCTION
      {
      ULONG BeginAddress;
      ULONG EndAddress;
      ULONG UnwindData;
      } RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;
    *)
    BeginAddress: ULONG;
    EndAddress: ULONG;
    UnwindData: ULONG;
  end;
{$EXTERNALSYM _RUNTIME_FUNCTION}

  RUNTIME_FUNCTION = _RUNTIME_FUNCTION;
{$EXTERNALSYM RUNTIME_FUNCTION}
  TRuntimeFunction = _RUNTIME_FUNCTION;
  PRuntimeFunction = ^TRuntimeFunction;

  { Unfortunately Delphi does not support bit manipulation in record.
    So I implemented properties to access fields those originally used
    bit manipulation. }

{$WARN UNSUPPORTED_CONSTRUCT OFF}

  [ExcludeField(Op)]
  _UNWIND_CODE = record
    (*
      typedef union _UNWIND_CODE {
      struct {
      UBYTE CodeOffset;
      UBYTE UnwindOp : 4;
      UBYTE OpInfo   : 4;
      };
      USHORT FrameOffset;
      } UNWIND_CODE, *PUNWIND_CODE;
    *)
  private
    function GetUnwindOp: Byte; // Unwind operation code
    function GetOpInfo: Byte; // Operation info
  public
    property UnwindOp: Byte read GetUnwindOp;
    property OpInfo: Byte read GetOpInfo;
    case Integer of
      0: (CodeOffset { Offset in prolog } , Op: Byte);
      1: (FrameOffset: USHORT);
  end;
{$EXTERNALSYM _UNWIND_CODE}

  UNWIND_CODE = _UNWIND_CODE;
{$EXTERNALSYM UNWIND_CODE}
  TUnwindCode = _UNWIND_CODE;
  PUnwindCode = ^TUnwindCode;

  [ExcludeField(VF, Frame)]
  _UNWIND_INFO = packed record
    (*
      typedef struct _UNWIND_INFO {
      UBYTE Version       : 3;
      UBYTE Flags         : 5;
      UBYTE SizeOfProlog;
      UBYTE CountOfCodes;
      UBYTE FrameRegister : 4;
      UBYTE FrameOffset   : 4;
      UNWIND_CODE UnwindCode[1];
      /*  UNWIND_CODE MoreUnwindCode[((CountOfCodes + 1) & ~1) - 1];
      *   union {
      *       OPTIONAL ULONG ExceptionHandler;
      *       OPTIONAL ULONG FunctionEntry;
      *   };
      *   OPTIONAL ULONG ExceptionData[]; */
      } UNWIND_INFO, *PUNWIND_INFO;
    *)
  private
    function GetFlags: Byte;
    function GetVersion: Byte;
    function GetFrameRegister: Byte;
    function GetFrameOffset: Byte;
  public
    property Version: Byte read GetVersion;
    property Flags: Byte read GetFlags;
    property FrameRegister: Byte read GetFrameRegister;
    property FrameOffset: Byte read GetFrameOffset;

  var
    { 3 first bits encode version.
      5 bits encode flags. }
    VF: Byte;
    SizeOfProlog: Byte;
    CountOfCodes: Byte;
    Frame: Byte;
    UNWIND_CODE: array [0 .. 0] of TUnwindCode;
  end;
{$EXTERNALSYM _UNWIND_INFO}

  UNWIND_INFO = _UNWIND_INFO;
{$EXTERNALSYM UNWIND_INFO}
  TUnwindInfo = _UNWIND_INFO;
  PUnwindInfo = ^TUnwindInfo;

{$WARN UNSUPPORTED_CONSTRUCT ON}
  { TUnwindInfo }

function TUnwindInfo.GetFlags: Byte;
begin
  Result := (VF and $F8) shr 3;
end;

function TUnwindInfo.GetVersion: Byte;
begin
  Result := VF and 3;
end;

function TUnwindInfo.GetFrameRegister: Byte;
begin
  Result := Frame and 3;
end;

function TUnwindInfo.GetFrameOffset: Byte;
begin
  Result := (VF and $F8) shr 3;
end;

{ TUnwindCode }

function TUnwindCode.GetOpInfo: Byte;
begin
  Result := (Op and $F0) shr 4;
end;

function TUnwindCode.GetUnwindOp: Byte;
begin
  Result := Op and $0F;
end;
{ =====>  <===== }

type
  { Delphi specific handler data. }

  { Keep synchronization with System ! }

  PExcScope = ^TExcScope;

  TExcScope = record
    BeginOffset: LongWord; // 32 bit RVA
    EndOffset: LongWord; // 32 bit RVA
    TableOffset: LongWord; // 32 bit RVA.
    // 0:TargetOffset=finally block
    // 1:TargetOffset=safecall catch block
    // 2:TargetOffset=catch block
    // other:TableOffset=TExcDesc
    TargetOffset: LongWord; // 32 bit RVA. start of finally/catch block.
    // TableOffset=0: signature is _TDelphiFinallyHandlerProc
    // TableOffset=1: signature is _TDelphiSafeCallCatchHandlerProc
    // TableOffset=2: Location to the catch block
    // other: TargetOffset=0
  end;

  PExcData = ^TExcData;

  TExcData = record
    ScopeCount: Integer;
    ScopeTable: array [0 .. 0 { ScopeCount-1 } ] of TExcScope;
  end;

{$ENDIF TABLE_BASED_EXCEPTIONS}

function EnumTryBlocks(ModuleHandle: HMODULE;
  CallBackFunction: TEnumTryCallBack; UserData: Pointer): Boolean;
{$IFDEF TABLE_BASED_EXCEPTIONS}
{ TODO: Implement C_specific_handler. }

  function RvaToVa(offset: Cardinal): Pointer;
  begin
    Result := Pointer(ModuleHandle + offset);
  end;

var
  Handlers: THandlersPtr;
  DelphiExceptionHandlerPtr: Pointer;
  ExceptionHandler: Pointer;
  NtHeaders: PImageNtHeaders64;
  NumberOfRuntimeFunctions: Cardinal;
  RtFunction: PRuntimeFunction;
  UnwindInfo: PUnwindInfo;
  P: Pointer;
  ExceptionHandlerData: PExcData;
  Scope: PExcScope;
  J: Integer;
  I: Integer;
  TryInfo: TTryBlockInfo;
{$ENDIF TABLE_BASED_EXCEPTIONS}
begin
  Result := False;
{$IFDEF TABLE_BASED_EXCEPTIONS}
  if not Assigned(CallBackFunction) then
    Exit;
  if ModuleHandle = 0 then
    ModuleHandle := GetModuleHandle(nil);
  GetDelphiExceptionHandlersPtr(Handlers);
  DelphiExceptionHandlerPtr := Handlers.SEHandler;
  NtHeaders := PeMapImageNtHeaders64(Pointer(ModuleHandle));
  with NtHeaders^.OptionalHeader.DataDirectory
    [IMAGE_DIRECTORY_ENTRY_EXCEPTION] do
  begin
    NumberOfRuntimeFunctions := Size div SizeOf(TRuntimeFunction);
    RtFunction := RvaToVa(VirtualAddress);
  end;
  for I := 1 to NumberOfRuntimeFunctions do
  begin
    UnwindInfo := RvaToVa(RtFunction^.UnwindData);
    if UnwindInfo^.Flags and (UNW_FLAG_EHANDLER or UNW_FLAG_UHANDLER) <> 0 then
    begin
      { For alignment purposes, UNWIND_CODE array will always have an even number of entries,
        with the final entry potentially unused. }
      { If the UNW_FLAG_EHANDLER is set, an exception handler offset will follow UNWIND_CODE array. }
      P := PByte(UnwindInfo) + SizeOf(TUnwindInfo);
      Inc(PByte(P), (UnwindInfo^.CountOfCodes) * SizeOf(TUnwindCode));
      ExceptionHandler := RvaToVa(PULONG(P)^);
      ExceptionHandlerData := Pointer(PByte(P) + SizeOf(ULONG));
      { ExceptionHandlerData is LANGUAGE SPECIFIC for ExceptionHandler !
        So we check first if ExceptionHandler is a DelphiExceptionHandler.

        TODO: Implement C_specific_handler. }
      if ExceptionHandler = DelphiExceptionHandlerPtr then
      begin
        { Begin of function that implemented try/finally|except block. }
        P := RvaToVa(RtFunction^.BeginAddress);

        TryInfo.Flags := 1; // New function.

        { Find all try block }
        for J := 0 to ExceptionHandlerData^.ScopeCount - 1 do
        begin
          Scope := @ExceptionHandlerData^.ScopeTable[J];
          case Scope^.TableOffset of
            0:
              begin
                { try..finally }
                // Keep processing.
                TryInfo.TryType := ttFinally;
              end;
            2:
              begin
                { try..except }
                // Keep processing.
                TryInfo.TryType := ttExcept;
              end
          else
            begin
              { Skip safecall }
              Continue;
            end;
          end;
          with TryInfo do
          begin
            FunctionStartAddress := P;
            FunctionEndAddress := RvaToVa(RtFunction^.EndAddress);
            TryStartAddress := RvaToVa(Scope^.BeginOffset);
            TryEndAddress := RvaToVa(Scope^.EndOffset);
            TryTargetAddress := RvaToVa(Scope^.TargetOffset);
            { It seems that all address are valid ! }

            { if not((NativeUInt(TryStartAddress) >= NativeUInt(FunctionStartAddress)) and (NativeUInt(TryStartAddress) < NativeUInt(FunctionEndAddress))) then
              Continue;
              if not((NativeUInt(TryEndAddress) > NativeUInt(FunctionStartAddress)) and (NativeUInt(TryEndAddress) < NativeUInt(FunctionEndAddress))) then
              Continue; }
          end;

          if not CallBackFunction(TryInfo, UserData) then
            Break;
          TryInfo.Flags := 0;
        end;
      end;
    end;
    Inc(RtFunction); // Next Runtime function.
  end;
  Result := True;
{$ENDIF TABLE_BASED_EXCEPTIONS}
end;

{$ENDREGION 'SEH'}

initialization

{$IFDEF TABLE_BASED_EXCEPTIONS }
//
Assert(SizeOf(TRuntimeFunction) = 12);
Assert(SizeOf(TUnwindCode) = 2);
Assert(SizeOf(TUnwindInfo) = 6);
{$ENDIF TABLE_BASED_EXCEPTIONS }

end.
