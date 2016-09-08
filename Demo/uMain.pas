unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls,
  System.Generics.Collections,
  System.Rtti,
  Vcl.Themes,
  DebugEngine.Core,
  DebugEngine.AsmRegUtils,
  DebugEngine.DebugInfo,
  DebugEngine.DebugUtils,
  DebugEngine.Disasm,
  DebugEngine.Trace,
  DebugEngine.HookException;

type
  TMain = class(TForm)
    LogMem: TMemo;
    Panel1: TPanel;
    BtnStackTrace: TButton;
    BtnTryTrace: TButton;
    BtnLegRegSnap: TButton;
    BtnDisasm: TButton;
    BtnAddrInfo: TButton;
    BtnEnumTryBlocks: TButton;
    BtnInsertDbgInfo: TButton;
    BtnRemoveDbgInfo: TButton;
    BtnVectorSnap: TButton;
    BtnTest: TButton;
    procedure BtnStackTraceClick(Sender: TObject);
    procedure BtnTryTraceClick(Sender: TObject);
    procedure BtnLegRegSnapClick(Sender: TObject);
    procedure BtnDisasmClick(Sender: TObject);
    procedure BtnAddrInfoClick(Sender: TObject);
    procedure BtnEnumTryBlocksClick(Sender: TObject);
    procedure BtnInsertDbgInfoClick(Sender: TObject);
    procedure BtnRemoveDbgInfoClick(Sender: TObject);
    procedure BtnVectorSnapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Trace;
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

procedure TMain.BtnStackTraceClick(Sender: TObject);
begin
  Trace;
end;

procedure TMain.BtnVectorSnapClick(Sender: TObject);
var
  VectorRegisters: TVectorRegisters;
  LCntx: TRttiContext;
  LType: TRttiType;
  LField: TRttiField;
  LFields: TArray<TRttiField>;
  XMMList: TStringList;
  YMMList: TStringList;
  ZMMList: TStringList;
  ZMM: TZMMRegister;
  PXMM: PXMMRegister;
  PYMM: PYMMRegister;
  Value: TValue;
  SL: TStringList;
  I: Integer;
  S: string;
begin

  LogMem.Clear;
  LogMem.Lines.BeginUpdate;

  XMMList := TStringList.Create;
  YMMList := TStringList.Create;
  ZMMList := TStringList.Create;
  SL := TStringList.Create;
  SL.Delimiter := '-';

  try
    SnapshotOfVectorRegisters(VectorRegisters);
    LCntx := TRttiContext.Create;
    LType := LCntx.GetType(TypeInfo(TVectorRegisters));
    LFields := LType.GetFields;
    for LField in LFields do
    begin
      ZeroMemory(@ZMM, SizeOf(ZMM));
      Value := LField.GetValue(@VectorRegisters);
      Value.ExtractRawData(@ZMM);
      SL.Clear;
      if Value.TypeInfo = TypeInfo(TXMMRegister) then
      begin
        PXMM := PXMMRegister(@ZMM);
        for I := Length(PXMM^.AsPackedInteger) - 1 downto 0 do
          SL.Add(IntToHex(PXMM^.AsPackedInteger[I], 8));
        S := Format('%s  =  [%s]', [LField.Name, SL.DelimitedText]);
        XMMList.Add(S);
      end
      else if Value.TypeInfo = TypeInfo(TYMMRegister) then
      begin
        PYMM := PYMMRegister(@ZMM);
        for I := Length(PYMM^.AsPackedInteger) - 1 downto 0 do
          SL.Add(IntToHex(PYMM^.AsPackedInteger[I], 8));
        S := Format('%s  =  [%s]', [LField.Name, SL.DelimitedText]);
        YMMList.Add(S);
      end
      else if Value.TypeInfo = TypeInfo(TZMMRegister) then
      begin
        for I := Length(ZMM.AsPackedInteger) - 1 downto 0 do
          SL.Add(IntToHex(ZMM.AsPackedInteger[I], 8));
        S := Format('%s  =  [%s]', [LField.Name, SL.DelimitedText]);
        ZMMList.Add(S);
      end;
    end;

    LogMem.Lines.Add('{ XMM Registers }');
    for I := 0 to XMMList.Count - 1 do
      LogMem.Lines.Add(XMMList[I]);

    LogMem.Lines.Add('{ YMM Registers }');
    for I := 0 to YMMList.Count - 1 do
      LogMem.Lines.Add(YMMList[I]);

    LogMem.Lines.Add('{ ZMM Registers }');
    for I := 0 to ZMMList.Count - 1 do
      LogMem.Lines.Add(ZMMList[I]);

  finally
    LogMem.Lines.EndUpdate;
    SL.Free;
    XMMList.Free;
    YMMList.Free;
    ZMMList.Free;
  end;
end;

procedure TMain.BtnTestClick(Sender: TObject);
var
  P: PDWORD;
begin
  //
  P := nil;
  try
    P^ := 1;
  except
    on E: Exception do
      LogMem.Text := E.StackTrace;
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  //
end;

procedure TMain.Trace;
begin
  LogMem.Clear;
  StackTrace(LogMem.Lines);
end;

procedure TMain.BtnInsertDbgInfoClick(Sender: TObject);
var
  AppFileName: String;
  S: String;
  MapFileName: string;
  SMapFileName: string;
  FooName: String;
begin
  LogMem.Clear;

  AppFileName := ParamStr(0);
  S := ChangeFileExt(AppFileName, '');
  FooName := ExtractFilePath(S) + 'foo.exe';
  MapFileName := S + DelphiMapFileExtension;
  SMapFileName := S + SMapFileExtension;
  with LogMem.Lines do
  begin
    if not FileExists(MapFileName) then
    begin
      Add(Format('"%s" file does not exist.', [MapFileName]));
      exit;
    end;
    Add(Format('Converting "%s" to SMAP format...', [MapFileName]));
    if ConvertMapToSMap(MapFileName, []) > 0 then
    begin
      Add(Format('"%s" generated successfully.', [SMapFileName]));
      Add(Format('Copying "%s" to "%s".', [AppFileName, FooName]));
      CopyFile(PChar(AppFileName), PChar(FooName), False);
      Add(Format('Starting inserting debug info into "%s"...', [FooName]));

      if InsertDebugInfo(FooName, SMapFileName, True) then
        Add('Done.')
      else
        Add('Error.')
    end;
  end;
  { To test :
    Run foo.exe and click Address info button.
    You should see [Resource or Section] map location
    instead of Disk in log. }
end;

function EnumTryCallBack(var Info: TTryBlockInfo; UserData: Pointer): Boolean;
var LInfo: TAddressInfo;
begin
  with Info, TMemo(UserData).Lines do
  begin
    if (Flags = 1) and GetAddressInfo(TryStartAddress, LInfo, aimSymbolName) then
    begin
      Add(Format('$%p  :  %s', [LInfo.SymbolAddress, LInfo.SymbolName]));
    end;
    Add(Format('TryAddress = $%p', [TryStartAddress]));
    if TryType = ttFinally then
      Add(Format('FinallyAddress = $%p', [TryEndAddress]))
    else
      Add(Format('ExceptAddress = $%p', [TryEndAddress]));
  end;
  Result := True;
end;

procedure TMain.BtnEnumTryBlocksClick(Sender: TObject);
begin
  LogMem.Lines.BeginUpdate;
  try
    EnumTryBlocks(0, EnumTryCallBack, LogMem);
  finally
    LogMem.Lines.EndUpdate;
  end;
end;

procedure TMain.BtnTryTraceClick(Sender: TObject);
begin
  LogMem.Clear;
  TraceTryBlocks(LogMem.Lines);
end;

procedure TMain.BtnLegRegSnapClick(Sender: TObject);
var Registers: TLegacyRegisters; LCntx: TRttiContext; LRegistersType: TRttiType;
  LAsRegisterType: TRttiType; LRegisterFields: TArray<TRttiField>; LRegisterField: TRttiField; LAsRegisterField: TRttiField;

  S: String; P: NativeUInt; Value: TValue;
begin
  LogMem.Clear;
  LogMem.Lines.BeginUpdate;
  try
    SnapshotOfLegacyRegisters(Registers);
    LCntx := TRttiContext.Create;
    LRegistersType := LCntx.GetType(TypeInfo(TLegacyRegisters));
    LRegisterFields := LRegistersType.GetFields;
    for LRegisterField in LRegisterFields do
    begin
      Value := LRegisterField.GetValue(@Registers);
      LAsRegisterType := LCntx.GetType(Value.TypeInfo);
      LAsRegisterField := LAsRegisterType.GetField('As' + LRegisterField.Name);
      Value.ExtractRawData(@P);
      Value := LAsRegisterField.GetValue(@P);
      S := Format('%s = $%.8x', [LRegisterField.Name, Value.AsType<NativeUInt>]);
      LogMem.Lines.Add(S);
    end;
  finally
    LogMem.Lines.EndUpdate;
  end;
end;

procedure TMain.BtnRemoveDbgInfoClick(Sender: TObject);
var
  AppFileName: string;
  FooName: string;
begin
  LogMem.Clear;
  AppFileName := ParamStr(0);
  FooName := ExtractFilePath(AppFileName) + 'foo.exe';
  with LogMem.Lines do
  begin
    Add(Format('Copying "%s" to "%s".', [AppFileName, FooName]));
    CopyFile(PChar(AppFileName), PChar(FooName), False);
    Add(Format('Starting removing debug from "%s"...', [FooName]));
    if RemoveDebugInfo(FooName, nil) then
      Add('Done')
    else
      Add('Error');
  end;
end;

procedure DisasmCallBack(var Info: TDisasmInfo; UserData: Pointer);
var S: String;
begin
  with TMemo(UserData).Lines, Info do
  begin
    S := Format('[$%p]:    %s', [Address, InstStr]);
    if not comment.IsEmpty then
      S := S + '    ; ' + comment;
    Add(S);
  end;
end;

procedure TMain.BtnDisasmClick(Sender: TObject);
var P: Pointer;
begin
  P := nil;
  LogMem.Clear;
  LogMem.Lines.BeginUpdate;
  try
    DisasmAndCommentFunction(@TMain.BtnLegRegSnapClick, P, DisasmCallBack, LogMem);
  finally
    LogMem.Lines.EndUpdate;
  end;
end;

procedure TMain.BtnAddrInfoClick(Sender: TObject);
var Info: TAddressInfo; P: Pointer;
begin
  LogMem.Clear;
  P := @TMain.BtnAddrInfoClick;
  if GetAddressInfo(P, Info) then
  begin
    with LogMem.Lines, Info do
    begin
      Add(Format('[MapLocation = %s]', [MapLocationToStr(DebugSource.Module.MapLocation)]));
      Add(Format('Address = $%p', [P]));
      Add(Format('SymbolAddress = $%p', [SymbolAddress]));
      Add('SymbolName = ' + SymbolName);
      Add('LineNumber = ' + IntToStr(LineNumber));
      Add('Unit = ' + UnitName);
      Add('SourceLocation = ' + SourceLocation);
    end;
  end;
end;

initialization

end.
