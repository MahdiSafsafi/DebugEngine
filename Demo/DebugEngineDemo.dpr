program DebugEngineDemo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  DebugEngine.AsmRegUtils in '..\Source\DebugEngine.AsmRegUtils.pas',
  DebugEngine.Core in '..\Source\DebugEngine.Core.pas',
  DebugEngine.DebugInfo in '..\Source\DebugEngine.DebugInfo.pas',
  DebugEngine.DebugUtils in '..\Source\DebugEngine.DebugUtils.pas',
  DebugEngine.Disasm in '..\Source\DebugEngine.Disasm.pas',
  DebugEngine.HookException in '..\Source\DebugEngine.HookException.pas',
  DebugEngine.MemoryHack in '..\Source\DebugEngine.MemoryHack.pas',
  DebugEngine.PeUtils in '..\Source\DebugEngine.PeUtils.pas',
  DebugEngine.Trace in '..\Source\DebugEngine.Trace.pas',
  UnivDisasm.Cnsts.Instructions in '..\Common\UnivDisasm\UnivDisasm.Cnsts.Instructions.pas',
  UnivDisasm.Cnsts.Mnemonics in '..\Common\UnivDisasm\UnivDisasm.Cnsts.Mnemonics.pas',
  UnivDisasm.Cnsts in '..\Common\UnivDisasm\UnivDisasm.Cnsts.pas',
  UnivDisasm.Cnsts.Regs in '..\Common\UnivDisasm\UnivDisasm.Cnsts.Regs.pas',
  UnivDisasm.Disasm in '..\Common\UnivDisasm\UnivDisasm.Disasm.pas',
  UnivDisasm.Internal.Common in '..\Common\UnivDisasm\UnivDisasm.Internal.Common.pas',
  UnivDisasm.Internal.Escape in '..\Common\UnivDisasm\UnivDisasm.Internal.Escape.pas',
  UnivDisasm.Internal.Prefixes in '..\Common\UnivDisasm\UnivDisasm.Internal.Prefixes.pas',
  UnivDisasm.Syntax.NilSyntax in '..\Common\UnivDisasm\UnivDisasm.Syntax.NilSyntax.pas',
  UnivDisasm.Syntax.UnivSyntax in '..\Common\UnivDisasm\UnivDisasm.Syntax.UnivSyntax.pas',
  UnivDisasm.Syntax.Utils in '..\Common\UnivDisasm\UnivDisasm.Syntax.Utils.pas',
  UnivDisasm.SyntaxManager in '..\Common\UnivDisasm\UnivDisasm.SyntaxManager.pas',
  UnivDisasm.Utils in '..\Common\UnivDisasm\UnivDisasm.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
