// **************************************************************************************************
// DD command line.
//
// https://github.com/MahdiSafsafi/DebugEngine

// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
//
//
// The Initial Developer of the Original Code is Mahdi Safsafi.
// Portions created by Mahdi Safsafi . are Copyright (C) 2016-2019 Mahdi Safsafi.
// All Rights Reserved.
//
// **************************************************************************************************

program DD;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  DebugEngine.Core in '..\..\..\Source\DebugEngine.Core.pas',
  DebugEngine.DebugInfo in '..\..\..\Source\DebugEngine.DebugInfo.pas',
  DebugEngine.DebugUtils in '..\..\..\Source\DebugEngine.DebugUtils.pas',
  DebugEngine.PeUtils in '..\..\..\Source\DebugEngine.PeUtils.pas';

procedure DoHelp;
begin
  Writeln('-----------------------------------------------');
  Writeln('DD command line tool.');
  Writeln('https://github.com/MahdiSafsafi/DebugEngine');
  Writeln('-----------------------------------------------');

  Writeln('Usage: DD [Command][Options][AppFile,MapFile]');
  Writeln('Command:');
  Writeln('-h = Display help.');
  Writeln('-c = Convert Delphi map to smap file format.');
  Writeln('-i = Insert debug info (smap) into the target application.');
  Writeln('-r = Remove Delphi debug info from the target application.');
  Writeln('Options:');
  Writeln('-p = Compress the smap file.');
  Writeln('-s = If possible, insert debug info into a new section.');
  Writeln('AppFile = Executable file.');
  Writeln('MapFile = If the command is -c then this should be a Delphi map file. If the -i is used then it should be a SMAP file.');

  Writeln('');
  Writeln('Example:');
  Writeln('Conver Delphi map to smap file:');
  Writeln('DD -c -p "MyApp.map"');
  Writeln('Inserting debug info:');
  Writeln('DD -i "MyApp.exe" "MyApp.smap"');
end;

const
  COMMAND_CONVERT = 1;
  COMMAND_DBG_INSERT = 2;
  COMMAND_DBG_REMOVE = 3;

var
  I: Integer;
  Param: string;
  Options: TSMapOptions;
  Command: Integer;
  F1: string;
  F2: string;
  SectionFlavor: Boolean;

begin
  Options := [];
  Command := 0;
  F1 := EmptyStr;
  F2 := EmptyStr;
  SectionFlavor := False;
  try
    for I := 1 to ParamCount do
    begin
      Param := ParamStr(I).Trim;
      if Param = '-h' then
      begin
        DoHelp;
        Exit;
      end;
      if Param = '-c' then
      begin
        Command := COMMAND_CONVERT;
        Continue;
      end;
      if Param = '-i' then
      begin
        Command := COMMAND_DBG_INSERT;
        Continue;
      end;
      if Param = '-r' then
      begin
        Command := COMMAND_DBG_REMOVE;
        Continue;
      end;
      if Param = '-p' then
      begin
        Options := [moCompress];
        Continue;
      end;
      if Param = '-s' then
      begin
        SectionFlavor := True;
        Continue;
      end;
      if F1.IsEmpty then
      begin
        F1 := Param;
        Continue;
      end;
      if F2.IsEmpty then
      begin
        F2 := Param;
        Continue;
      end;
    end;

    if (Command > 0) and (not FileExists(F1)) then
    begin
      Writeln(Format('"%s does not exist."', [F1]));
      Exit;
    end;

    case Command of
      COMMAND_CONVERT:
        begin
          if ConvertMapToSMap(F1, Options) > 0 then
            Writeln('smap file generated successfully.')
          else
            Writeln('Failed to generate smap file.');
          Exit;
        end;
      COMMAND_DBG_INSERT:
        begin
          if FileExists(F2) then
          begin
            if InsertDebugInfo(F1, F2, SectionFlavor) then
              Writeln('Debug info inserted successfully.')
            else
              Writeln('Failed to insert debug info.');
          end
          else
            Writeln(Format('"%s does not exist."', [F2]));
          Exit;
        end;
      COMMAND_DBG_REMOVE:
        begin
          if RemoveDebugInfo(F1, nil) then
            Writeln('Delphi debug info removed successfully from app.')
          else
            Writeln('Failed to remove debug info.');
          Exit;
        end
    else
      begin
        DoHelp;
        Exit;
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
