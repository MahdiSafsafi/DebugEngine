// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.Disasm
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.Disasm.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

unit DebugEngine.Disasm;

interface

{$I DebugEngine.inc}

uses
  WinApi.Windows,
  System.SysUtils;

type
  TDisasmInfo = record
    Address: Pointer;
    InstStr: string;
    Comment: string;
  end;

  PDisasmInfo = ^TDisasmInfo;

  TDisasmCallBack = procedure(var Info: TDisasmInfo; UserData: Pointer);

  TGetCallTargetAddressOptions = set of (ctFollowJmp, ctBaseOnPrvIns);
  /// <summary> Return first ret instruction address.
  /// </summary>
function GetRetAddress(Address: Pointer): Pointer;

/// <summary> Return last ret instruction address.
/// </summary>
function GetLastRetAddress(Address: Pointer): Pointer;

/// <summary> Get caller address from return address.
/// </summary>
/// <param name="ReturnAddress"> Address of the first instruction after the call instruction.
/// </param>
/// <param name="CallerAddress"> If the function succeeds, this variable will hold address of call instruction.
/// </param>
/// <returns> Return <c>True</c> if the instruction above return address is CALL.
/// </returns>
function GetCallerAddress(ReturnAddress: Pointer; out CallerAddress: Pointer): Boolean;

/// <summary> Get <c>CALL/JMP</c> target address.
/// </summary>
/// <param name="CallAddress"> Address of <c>CALL/JMP</c> instruction.
/// </param>
/// <param name="CallTargetAddress"> If the function succeeds, this variable will hold the target <c>CALL/JMP</c> address.
/// </param>
/// <param name="Options"> See <see cref="TGetCallTargetAddressOptions"/>.
/// </param>
/// <returns> If the function succeeds, the return value is True.
/// </returns>
function GetCallTargetAddress(CallAddress: Pointer; out CallTargetAddress: Pointer; Options: TGetCallTargetAddressOptions): Boolean;

/// <summary> Check if address in function's body.
/// </summary>
/// <param name="Address"> Address to test.
/// </param>
/// <param name="FunctionAddress"> Address of function.
/// </param>
/// <returns> It returns <c>True</c> if the address in the function range.
/// Otherwise it returns <c>False</c>.
/// </returns>
function IsAddressInFunctionBody(Address: PByteArray; FunctionAddress: PByte): Boolean;

/// <summary> Disasm and comment function.
/// </summary>
/// <param name="FunctionStartAddress"> The address where the function will start disassembling.
/// </param>
/// <param name="FunctionEndAddress"> The end address of the function where the disassembling will stop.
/// If not specified (nil), the disassembling will be stoped at the first ret instruction and this variable will
/// hold the ret instruction address.
/// </param>
/// <param name="CallBackFunction"> A pointer to a call back function, this one will be called by <c>DisasmAndCommentFunction</c>
/// each time the function disasm an instruction.
/// </param>
/// <param name="UserData"> Optional data to pass to the callback function.
/// </param>
/// <returns> If the function succeeds, the return value is True.
/// </returns>
function DisasmAndCommentFunction(FunctionStartAddress: Pointer; var FunctionEndAddress: Pointer; CallBackFunction: TDisasmCallBack; UserData: Pointer)
  : Boolean;

implementation

uses
  UnivDisasm.Disasm,
  UnivDisasm.Cnsts,
  UnivDisasm.Cnsts.Regs,
  UnivDisasm.Cnsts.Instructions,
  UnivDisasm.Syntax.NilSyntax,
  DebugEngine.DebugInfo,
  DebugEngine.MemoryHack;

function GetCallerAddress(ReturnAddress: Pointer; out CallerAddress: Pointer): Boolean;
const
  { ADSIZE_PREFIX + REX_PREFIX + 0xFF + xx010xxx + SIB + DISP32 = 9 byte. }
  MAX_CALL_SIZE = $09;
var
  Ins: TInstruction;
  I: Integer;
begin
  CallerAddress := ReturnAddress;
  { Start from zero just in case
    call instruction uses duplicated prefix
    So Call size = MAX_CALL_SIZE + 1 }
  for I := 0 to MAX_CALL_SIZE do
  begin
    Dec(PByte(CallerAddress));
    Ins := TInstruction.Create;
    Ins.Arch := CPUX;
    Ins.Syntax := SX_NIL_SYNTAX;
    Ins.Addr := CallerAddress;
    Disasm(@Ins);
    Ins.Free;
    if (Ins.InstID = INST_ID_CALL) and //
      (NativeUInt(Ins.NextInst) = NativeUInt(ReturnAddress)) then
      Exit(True);
  end;
  CallerAddress := nil;
  Result := False;
end;

function GetCallTargetAddress(CallAddress: Pointer; out CallTargetAddress: Pointer; Options: TGetCallTargetAddressOptions): Boolean;
const
  MAX_INST_SIZE = 15;
var
  Ins: TInstruction;
  P: PByte;
  Imm: Int64;
  Reg: Integer;
begin
  Reg := REG_NIL;
  CallTargetAddress := nil;
  P := CallAddress;
  while True do
  begin
    Ins := TInstruction.Create;
    Ins.Arch := CPUX;
    Ins.Syntax := SX_NIL_SYNTAX;
    Ins.Addr := P;
    Disasm(@Ins);
    Ins.Free;
    P := Ins.DstAddr.Addr;
    if not Assigned(P) then
    begin
      Reg := Ins.Arg1.Reg;
      Break;
    end;
    CallTargetAddress := Ins.DstAddr.Addr;
    if Assigned(CallTargetAddress) and (not(ctFollowJmp in Options)) then
      Break;
  end;
  { TODO: Support memory
    mov regX,$xxxxxxxx && call regX => Done!
    mov regX,$xxxxxxxx && call [regX] => Not yet! }
  if (ctBaseOnPrvIns in Options) and (Reg <> REG_NIL) and not Assigned(CallTargetAddress) then
  begin
    P := CallAddress;
    Dec(PByte(P), MAX_INST_SIZE);
    while True do
    begin
      if NativeUInt(P) >= NativeUInt(CallAddress) then
        Break;
      Ins := TInstruction.Create;
      Ins.Arch := CPUX;
      Ins.Syntax := SX_NIL_SYNTAX;
      Ins.Addr := P;
      Disasm(@Ins);
      Inc(P);
      Ins.Free;
      if (Ins.InstID = INST_ID_MOV) and (Ins.Arg1.Reg = Reg) and (Ins.NextInst = CallAddress) then
      begin
        Imm := Ins.Arg2.Imm.Value;
        if Imm <> 0 then
        begin
          CallTargetAddress := Pointer(Imm);
          Break;
        end;
      end;
    end;
  end;
  Result := Assigned(CallTargetAddress);
end;

function GetRetAddress(Address: Pointer): Pointer;
var
  Ins: TInstruction;
  P: Pointer;
begin
  P := Address;
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
    if Ins.InstID = INST_ID_RET then
      Exit(P);

    P := Ins.NextInst;
  end;
  Result := nil;
end;

function GetLastRetAddress(Address: Pointer): Pointer;
var
  Ins: TInstruction;
  P: Pointer;
  Jmp: Pointer;
  Target: Pointer;
begin
  P := Address;
  Jmp := nil;
  Result := nil;
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
    case Ins.InstID of
      INST_ID_JB .. INST_ID_JLE, INST_ID_JNB .. INST_ID_JZ:
        begin
          Target := Ins.DstAddr.Addr;
          if (NativeUInt(Target) > NativeUInt(Jmp)) then
            Jmp := Target;
        end;
      INST_ID_RET:
        begin
          Result := P;
          if (NativeUInt(P) > NativeUInt(Jmp)) then
            Break
        end;
    end;
    P := Ins.NextInst;
  end;
end;

function IsAddressInFunctionBody(Address: PByteArray; FunctionAddress: PByte): Boolean;
{ The initial code was taken from JclDebug,
  However, I rewrite the code from the scratch
  to integrate UnivDisasm library and
  add support for more JMP types. }
  function GetReg(Value: Byte): Byte; inline;
  begin
    Result := (Value and $38) shr 3;
  end;

var
  Ins: TInstruction;
  Tmp: PByteArray;
begin
  while PByte(Address) > FunctionAddress do
  begin
    if IsBadCodePtr(Address) then
      Exit(False);

    { Sequence of Int3. }
    if (PByte(Address)^ = $CC) and (PDWORD(PByte(Address) + 1)^ = $CCCCCCCC) then
      Exit(False);
    { Ret $xxxx + sequence of (Int3/Nop). }
    if (Address^[0] = $C2) and ((Address^[3] or Address^[4] = $CC) or (Address^[3] or Address^[4] = $90)) then
      Exit(False);
    { Ret + sequence of (Int3/Nop). }
    if (Address^[0] = $C3) and ((Address^[1] or Address^[2] = $CC) or (Address^[1] or Address^[2] = $90)) then
      Exit(False);
    { If we didn't found a ret and there is a jmp, that means that we are running out of the function body. }

    { JMP near offset8 + sequence of (Int3/Nop). }
    if (Address^[0] = $EB) and ((Address^[2] or Address^[3] = $CC) or (Address^[2] or Address^[3] = $90)) then
      Exit(False);

    if PByte(Address)^ = $66 then // opsize prefix.
    begin
      { JMP offset16 + sequence of (Int3/Nop). }
      if (Address^[1] = $E9) and ((Address^[4] or Address^[5] = $CC) or (Address^[4] or Address^[5] = $90)) then
        Exit(False);

{$IFDEF CPUX86}
      { JMP far (seg:offset16) }
      if (Address^[1] = $EA) and (Address^[6] in [$CC, $90]) then
        Exit(False);
{$ENDIF CPUX86}
      { JMP Ev + sequence of (Int3/Nop). }
      if (Address^[1] = $FF) and (GetReg(Address^[2]) = $04) then
      begin
        { Limit the disasm call => Handle it last. }
        Ins := TInstruction.Create;
        Ins.Arch := CPUX;
        Ins.Syntax := SX_NIL_SYNTAX;
        Ins.Addr := PByte(Address);
        Disasm(@Ins);
        Ins.Free;
        Tmp := PByteArray(Ins.NextInst);
        if (Ins.InstID = INST_ID_JMP) and ((Tmp^[0] or Tmp^[1] = $CC) or (Tmp^[0] or Tmp^[1] = $90)) then
          Exit(False);
      end;
    end else begin
      { JMP offset32 + sequence of (Int3/Nop). }
      if (Address^[0] = $E9) and ((Address^[5] or Address^[6] = $CC) or (Address^[5] or Address^[6] = $90)) then
        Exit(False);
{$IFDEF CPUX86}
      { JMP far (seg:offset32) }
      if (Address^[0] = $EA) and (Address^[7] in [$CC, $90]) then
        Exit(False);
{$ENDIF CPUX86}
      { JMP Ev + sequence of (Int3/Nop). }
      if (Address^[0] = $FF) and (GetReg(Address^[1]) = $04) then
      begin
        { Limit the disasm call => Handle it last. }
        Ins := TInstruction.Create;
        Ins.Arch := CPUX;
        Ins.Syntax := SX_NIL_SYNTAX;
        Ins.Addr := PByte(Address);
        Tmp := PByteArray(Ins.NextInst);
        if (Ins.InstID = INST_ID_JMP) and ((Tmp^[0] or Tmp^[1] = $CC) or (Tmp^[0] or Tmp^[1] = $90)) then
          Exit(False);
      end;
    end;
    Dec(PByte(Address));
  end;
  Result := True;
end;

function DisasmAndCommentFunction(FunctionStartAddress: Pointer; var FunctionEndAddress: Pointer; CallBackFunction: TDisasmCallBack; UserData: Pointer)
  : Boolean;
var
  Ins: TInstruction;
  P: Pointer;
  DisasmInfo: TDisasmInfo;
  S: String;
  LExceptAddr: Pointer;
  Info: TAddressInfo;

  function IsPtrInFunctionRange(P: Pointer): Boolean;
  begin
    Result := (NativeUInt(P) >= NativeUInt(FunctionStartAddress)) and (NativeUInt(P) <= NativeUInt(FunctionEndAddress));
  end;

  function GetMemoryPtrFromArg(const Arg: TArgument): Pointer;
  begin
    if Arg.Flags and AF_OFFSET = AF_OFFSET then
    begin
      Result := Pointer(Arg.Imm.Value);
{$IFDEF CPUX64}
      { On x64 sign is extended to 64 bit ! }
      Result := Pointer(NativeUInt(Result) and $00000000FFFFFFFF);
{$ENDIF CPUX64}
      Exit;
    end;
    if (Arg.Flags and AF_TYPE_MASK = AF_MEM) and (Ins.Disp.Value <> 0) then
    begin
      with Arg.Mem do
        if Scale + BaseReg + IndexReg = 0 then
        begin
          Result := Pointer(Ins.Disp.Value); // Sign extended !
          if Ins.Disp.Flags and DF_REL <> 0 then
            Result := Pointer(NativeUInt(Ins.NextInst) + NativeUInt(Result));
          Exit;
        end;
    end;
    if (Arg.Imm.Value <> 0) and (Arg.Imm.Size = SizeOf(Pointer)) then
    begin
      { We may accept that immediate can hold a valid pointer
        only if the immediate size is equal to the size of pointer. }
      if not IsBadCodePtr(Pointer(Arg.Imm.Value)) then
      begin
        Result := Pointer(Arg.Imm.Value);
        Exit;
      end;
      try
        { All IsBadString/Read must be handled ! }
        if not IsBadStringPtr(Pointer(Arg.Imm.Value), 4) then
        begin
          Result := Pointer(Arg.Imm.Value);
          Exit;
        end;
      except
        Exit(nil);
      end;
    end;
    Result := nil;
  end;

  function CommentArg(var Arg: TArgument): Boolean;
  var
    P: Pointer;
    // AClass: TClass;
    Info: TAddressInfo;
    StrType: TStrType;
    C: Integer;
    Skip: Boolean;
    S: string;
    L: Integer;

  begin
    P := GetMemoryPtrFromArg(Arg);
    Result := Assigned(P);
    if not Result then
      Exit;

    {
      if IsPtrClass(P) then
      begin
      AClass := TClass(PPointer(P)^);
      DisasmInfo.Comment := Format('Class (%s.%s)', [AClass.UnitName, AClass.ClassName]);
      Exit;
      end;

    }
    if GetAddressInfo(P, Info, aimSymbolName) and (Info.SymbolAddress = P) then
    begin
      with Info do
        S := Format('%s.%s', [UnitName, SymbolName]);
      S := S.Replace('..', '.');
      DisasmInfo.Comment := S;
      Exit;
    end;

    if (not IsPtrInFunctionRange(P)) then
    begin
      StrType := GetStrType(P, SizeOf(Pointer), 64);
      case StrType of
        stUnknown:
          begin
            try
              Skip := IsBadReadPtr(P, 8);
            except
              Skip := True;
            end;
            if not Skip then
            begin
              case Arg.Size of
                SIZE_1_BYTE:
                  begin
                    C := PByte(P)^;
                    { Could be a char ? }
                    if C in [Ord('0') .. Ord('9'), Ord('a') .. Ord('z'), Ord('A') .. Ord('Z')] then
                      DisasmInfo.Comment := Format('"%c"', [Char(C)])
                    else // Not a char.
                      DisasmInfo.Comment := Format('%d', [ShortInt(C)]);
                    Exit;
                  end;
                SIZE_2_BYTE:
                  begin
                    C := PWord(P)^;
                    { Could be a unicode char ? }
                    if C in [Ord('0') .. Ord('9'), Ord('a') .. Ord('z'), Ord('A') .. Ord('Z')] then
                      DisasmInfo.Comment := Format('"%c"', [Char(C)])
                    else // Not a unicode char.
                      DisasmInfo.Comment := (Format('%d', [PSmallInt(P)^]));
                    Exit;
                  end;
                SIZE_4_BYTE:
                  begin
{$IFDEF CPUX86}
                    { Could be a pointer ! }
                    if GetAddressInfo(PPointer(P)^, Info, aimSymbolName) then
                    begin
                      with Info do
                        DisasmInfo.Comment := (Format('%s.%s', [UnitName, SymbolName]));
                    end
                    else
                      DisasmInfo.Comment := (Format('%d', [PInteger(P)^]));
{$ELSE !CPUX86}
                    { Not a pointer ! }
                    DisasmInfo.Comment := (Format('%d', [PInteger(P)^]));
{$ENDIF CPUX86}
                    Exit;
                  end;
                SIZE_8_BYTE:
                  begin
{$IFDEF CPUX64}
                    { Could be a pointer ! }
                    if GetAddressInfo(PPointer(P)^, Info, aimSymbolName) then
                    begin
                      with Info do
                        DisasmInfo.Comment := (Format('%s.%s', [UnitName, SymbolName]));
                    end
                    else
                      DisasmInfo.Comment := (Format('%d', [PInt64(P)^]));
{$ELSE !CPUX64}
                    { Not a pointer ! }
                    DisasmInfo.Comment := (Format('%d', [PInt64(P)^]));
{$ENDIF CPUX64}
                    Exit;
                  end;
              end;
            end;
          end;
        stPAnsiChar:
          begin
            DisasmInfo.Comment := Format('PAnsiChar "%s"', [string(PAnsiChar(P))]);
            // Exit;
          end;
        stPChar:
          begin
            DisasmInfo.Comment := Format('PChar "%s"', [string(PChar(P))]);
            // Exit;
          end;
        stAnsiString:
          begin
            DisasmInfo.Comment := Format('AnsiString "%s"', [string(AnsiString(P))]);
            Exit;
          end;
        stString:
          begin
            DisasmInfo.Comment := Format('String "%s"', [string(P)]);
            Exit;
          end;
      end;
    end;
    if GetAddressInfo(P, Info, aimSymbolName) then
    begin
      with Info do
      begin
        S := Format('%s.%s', [UnitName, SymbolName]);
        L := NativeUInt(P) - NativeUInt(SymbolAddress);
        if L <> 0 then
          S := S + ' + $' + IntToHex(L, 2);
      end;
      DisasmInfo.Comment := S;
      Exit;
    end;
  end;

begin
  Result := False;
  if not Assigned(CallBackFunction) then
    Exit;
  if not Assigned(FunctionStartAddress) then
    Exit;
  if not Assigned(FunctionEndAddress) then
    FunctionEndAddress := GetRetAddress(FunctionStartAddress);
  if not Assigned(FunctionEndAddress) then
    Exit;
  if NativeUInt(FunctionStartAddress) >= NativeUInt(FunctionEndAddress) then
    Exit;

  LExceptAddr := ExceptAddr;
  P := FunctionStartAddress;
  while NativeUInt(P) <= NativeUInt(FunctionEndAddress) do
  begin
    Ins := TInstruction.Create;
    Ins.Arch := CPUX;
    Ins.Addr := P;
    Disasm(@Ins);
    S := string(Ins.InstStr);
    Ins.Free;

    ZeroMemory(@DisasmInfo, SizeOf(DisasmInfo));

    if Assigned(Ins.DstAddr.Addr) then
    begin
      if IsPtrInFunctionRange(Ins.DstAddr.Addr) then
      begin
        { Local address }
        S := S.Replace('@', '@local_');
        DisasmInfo.Comment := 'Entry + $' + IntToHex(NativeUInt(Ins.DstAddr.Addr) - NativeUInt(FunctionStartAddress), 2);
      end
      else if GetAddressInfo(Ins.DstAddr.Addr, Info, aimSymbolName) then
      begin
        S := S.Remove(S.IndexOf('@'), S.length);
        S := Format('%s %s', [S, Info.SymbolName]);
      end;
    end;

    DisasmInfo.Address := P;
    DisasmInfo.InstStr := S;
    if P <> LExceptAddr then
    begin
      if not CommentArg(Ins.Arg1) then
        if not CommentArg(Ins.Arg2) then
          if not CommentArg(Ins.Arg3) then
            CommentArg(Ins.Arg4);
    end
    else
      DisasmInfo.Comment := 'Error';

    CallBackFunction(DisasmInfo, UserData);

    P := Ins.NextInst; // Next instruction.

    { Avoid memory leaks => Destroy strings. }
    // FinalizeRecord(@DisasmInfo, TypeInfo(TDisasmInfo));
    Finalize(DisasmInfo);
  end;

  Result := True;
end;

end.
