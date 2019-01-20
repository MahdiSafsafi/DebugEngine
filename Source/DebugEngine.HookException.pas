// **************************************************************************************************
// Delphi DebugEngine.
// Unit DebugEngine.HookException
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is DebugEngine.HookException.pas.
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

unit DebugEngine.HookException;

interface

uses
  System.Classes,
  System.SysUtils;

procedure InstallExceptionHook;
procedure RemoveExceptionHook;

implementation

uses
  DebugEngine.Core,
  DebugEngine.Trace;

function GetStackInfoString(Info: Pointer): string;
begin
  Result := string(PChar(Info));
end;

function GetExceptionStackInfo(P: PExceptionRecord): Pointer;
var
  SL: TStringList;
  S: String;
  L: Integer;
  StackTrace: TCallTrace;
  StackInfo: TStackInfo;
  PItem: PStackItem;
  Item: TStackItem;
  LExceptionAddress: Pointer;
  I: Integer;
begin
  GetStackInfo(StackInfo);
  SL := TStringList.Create;
  S := '';
  LExceptionAddress := P^.ExceptionAddress;
  if GetStackTraceError = 0 then
  begin
    { First caller is exception address. }
    FillChar(Item, SizeOf(Item), #00);
    Item.CallAddress := LExceptionAddress;
    Item.Info.Address := LExceptionAddress;
    SL.Add(LogCall(@Item));
    StackTrace := TCallTrace.Create;
    StackTrace.Options:= [soUseFirstCallOnEbp{,soRebuildBrokenEbpChain,soDropCurrentEbpChain}];
    try
      StackTrace.StackInfo := StackInfo;
      StackTrace.Trace;
      for I := 0 to StackTrace.Count - 1 do
      begin
        PItem := StackTrace.Items[I];
        SL.Add(LogCall(PItem));
      end;
    finally
      StackTrace.Free;
    end;
    S := SL.Text;
  end;
  L := ((S.Length + 1) * 2);
  GetMem(Result, L);
  Move(PChar(S)^, Result^, L);
  SL.Free;
end;

procedure CleanUpStackInfo(Info: Pointer);
begin
  FreeMem(Info);
end;

procedure InstallExceptionHook;
begin
  Exception.GetExceptionStackInfoProc := GetExceptionStackInfo;
  Exception.GetStackInfoStringProc := GetStackInfoString;
  Exception.CleanUpStackInfoProc := CleanUpStackInfo;
end;

procedure RemoveExceptionHook;
begin
  Exception.GetExceptionStackInfoProc := nil;
  Exception.GetStackInfoStringProc := nil;
  Exception.CleanUpStackInfoProc := nil;
end;

initialization

InstallExceptionHook;

finalization

RemoveExceptionHook;

end.
