program LCV;

{$mode objfpc}{$H+}

{$include LCV.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, unitMain, dataio, unitphasedialog, unitdftdialog,
  unitdftparamdialog, unitdcdft, DoLongOp, dftthread, common, unitfitparamdialog
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormPhaseDialog, FormPhaseDialog);
  Application.CreateForm(TFormDFTDialog, FormDFTDialog);
  Application.Run;
end.

