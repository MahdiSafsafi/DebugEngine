object Main: TMain
  Left = 0
  Top = 0
  Caption = 'DebugEngineDemo'
  ClientHeight = 387
  ClientWidth = 547
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LogMem: TMemo
    Left = 0
    Top = 0
    Width = 547
    Height = 219
    Align = alClient
    Lines.Strings = (
      'LogMem')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 219
    Width = 547
    Height = 168
    Align = alBottom
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object BtnStackTrace: TButton
      Left = 368
      Top = 22
      Width = 163
      Height = 25
      Caption = 'Stack Trace'
      TabOrder = 0
      OnClick = BtnStackTraceClick
    end
    object BtnTryTrace: TButton
      Left = 368
      Top = 53
      Width = 163
      Height = 25
      Caption = 'Trace try blocks'
      TabOrder = 1
      OnClick = BtnTryTraceClick
    end
    object BtnLegRegSnap: TButton
      Left = 8
      Top = 22
      Width = 185
      Height = 25
      Caption = 'Snapshot of legacy registers'
      TabOrder = 2
      OnClick = BtnLegRegSnapClick
    end
    object BtnDisasm: TButton
      Left = 8
      Top = 85
      Width = 185
      Height = 25
      Caption = 'Disasm and comment'
      TabOrder = 3
      OnClick = BtnDisasmClick
    end
    object BtnAddrInfo: TButton
      Left = 199
      Top = 22
      Width = 163
      Height = 25
      Caption = 'Address info'
      TabOrder = 4
      OnClick = BtnAddrInfoClick
    end
    object BtnEnumTryBlocks: TButton
      Left = 368
      Top = 84
      Width = 163
      Height = 25
      Caption = 'Enum try blocks'
      TabOrder = 5
      OnClick = BtnEnumTryBlocksClick
    end
    object BtnInsertDbgInfo: TButton
      Left = 199
      Top = 85
      Width = 163
      Height = 25
      Caption = 'Insert debug info'
      TabOrder = 6
      OnClick = BtnInsertDbgInfoClick
    end
    object BtnVectorSnap: TButton
      Left = 8
      Top = 54
      Width = 185
      Height = 25
      Caption = 'Snapshot of vector registers'
      TabOrder = 7
      OnClick = BtnVectorSnapClick
    end
    object BtnRemoveDbgInfo: TButton
      Left = 199
      Top = 116
      Width = 163
      Height = 25
      Caption = 'Remove debug info'
      TabOrder = 8
      OnClick = BtnRemoveDbgInfoClick
    end
    object BtnTest: TButton
      Left = 368
      Top = 116
      Width = 163
      Height = 25
      Caption = 'Test exception'
      TabOrder = 9
      OnClick = BtnTestClick
    end
    object BtnSymAddr: TButton
      Left = 199
      Top = 54
      Width = 163
      Height = 25
      Caption = 'Address of symbol'
      TabOrder = 10
      OnClick = BtnSymAddrClick
    end
    object BtnSizeOfProc: TButton
      Left = 8
      Top = 116
      Width = 185
      Height = 25
      Caption = 'Size of proc'
      TabOrder = 11
      OnClick = BtnSizeOfProcClick
    end
  end
end
