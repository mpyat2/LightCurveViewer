unit DoLongOp;

{$mode ObjFPC}{$H+}

{$include LCV.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

const
  WM_DOOPERATION = WM_USER + 101;

type
  TProgressCaptionProc = procedure (const Msg: string) of object;
  TLongOpProc = function (ArgsPtr: Pointer; ProgressCaptionProc: TProgressCaptionProc): Integer of object;

type
  TFormLongOperation = class(TForm)
    LabelProgress: TLabel;
    ButtonCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FStarted: Boolean;
    FOperationDone: Boolean;
    FLongOperation: TLongOpProc;
    FArgsPtr: Pointer;
    FResult: Integer;
    FExceptionMessage: string;
    FEAbortException: Boolean;
    procedure WmDoOperation(var Msg: TMessage); message WM_DOOPERATION;
    procedure ProgressCaptionProc(const Msg: string);
    { Private declarations }
  public
    { Public declarations }
  end;

type
  ELongOperationError = class(Exception);

function DoLongOperation(LongOperation: TLongOpProc;
                         ArgsPtr: Pointer;
                         OnCancel: TNotifyEvent = nil;
                         const Caption: string = '';
                         X: Integer = -1;
                         Y: Integer = -1): Integer;

procedure DisableLongOpCancelButton;

implementation

{$R *.lfm}

const
  SLongOpInternalError = 'DoLongOperation: Internal error';
  SLongOpUnknownError = 'Unknown error (exception object type is %s)';
  SLabelProgressCaption = 'Please wait...';
  SButtonCancelCaption = 'Cancel';

var
  FormLongOperation: TFormLongOperation = nil;

function DoLongOperation(LongOperation: TLongOpProc;
                         ArgsPtr: Pointer;
                         OnCancel: TNotifyEvent = nil;
                         const Caption: string = '';
                         X: Integer = -1;
                         Y: Integer = -1): Integer;
var
  F: TForm;
begin
  Result := 0;
  FormLongOperation := TFormLongOperation.Create(Application);
  try
    FormLongOperation.FArgsPtr := ArgsPtr;
    if (X >= 0) and (Y >= 0) then begin
      FormLongOperation.Position := poDesigned;
      FormLongOperation.Left := X;
      FormLongOperation.Top := Y;
    end;
    FormLongOperation.FLongOperation := LongOperation;
    FormLongOperation.ButtonCancel.Enabled := Assigned(OnCancel);
    FormLongOperation.ButtonCancel.Visible := Assigned(OnCancel);
    FormLongOperation.ButtonCancel.OnClick := OnCancel;
    FormLongOperation.Caption := Caption;
    FormLongOperation.ShowModal;
    if FormLongOperation.FOperationDone then begin  // #FIX#20051015#1#
      Result := FormLongOperation.FResult;
    end
    else begin // Exception occured!
      if FormLongOperation.FEAbortException then
        Abort
      else  
      if FormLongOperation.FExceptionMessage <> '' then
        raise ELongOperationError.Create(FormLongOperation.FExceptionMessage)
      else
        raise ELongOperationError.Create(SLongOpInternalError);
    end;
  finally
    F := FormLongOperation;
    FormLongOperation := nil;
    F.Release; (*X*) // #FIX#20051015#1#
  end;
end;

procedure DisableLongOpCancelButton;
begin
  if Assigned(FormLongOperation) then
    FormLongOperation.ButtonCancel.Enabled := False;
end;

{ TFormLongOperation }

procedure TFormLongOperation.FormCreate(Sender: TObject);
begin
  FStarted := False;
  FOperationDone := False;
  FLongOperation := nil;
  FArgsPtr := nil;
  FResult := 0;
  FExceptionMessage := '';
  FEAbortException := False;
  LabelProgress.Caption := SLabelProgressCaption;
  ButtonCancel.Caption := SButtonCancelCaption;
end;

procedure TFormLongOperation.FormActivate(Sender: TObject);
begin
  if not FStarted then begin
    FStarted := True;
    PostMessage(Handle, WM_DOOPERATION, 0, 0);
  end;
end;

procedure TFormLongOperation.WmDoOperation(var Msg: TMessage);
begin
  ModalResult := mrNone;
  try
    FOperationDone := False;
    if Assigned(FLongOperation) then FResult := FLongOperation(FArgsPtr, @ProgressCaptionProc);
    FOperationDone := True;              // #FIX#20051015#1#
    ButtonCancel.OnClick := nil;   (*1*) // #FIX#20051015#1#
    ButtonCancel.Enabled := False; (*2*) // #FIX#20051015#1#
{
    Comment to #FIX#20051015#1#.
    If not ButtonCancel.Enabled := False, the Cancel button event can be fired
    after ModalResult := mrOk (*3*).
    F.Free (*X*) is replaced with F.Release -- just in case.
    Testing FOperationDone to ensure the operation is completed.
}
    ModalResult := mrOk;           (*3*)
  except
    on E: Exception do begin
      FExceptionMessage := E.Message;
      if FExceptionMessage = '' then FExceptionMessage := Format(SLongOpUnknownError, [E.ClassName]);
      if E is EAbort then FEAbortException := True;
      ModalResult := mrCancel;
    end;
    else begin
      FExceptionMessage := Format(SLongOpUnknownError, [ExceptObject.ClassName]);
      ModalResult := mrCancel;
    end;
  end;
end;

procedure TFormLongOperation.ProgressCaptionProc(const Msg: string);
begin
  LabelProgress.Caption := Msg;
end;

end.
