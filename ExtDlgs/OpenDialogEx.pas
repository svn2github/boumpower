unit OpenDialogEx;

interface

uses
  SysUtils, Classes, Dialogs, ExtCtrls;

type
  TOpenDialogEx = class(TOpenDialog)
  private
    FOnCreate  : TNotifyEvent;
    FOnDestroy : TNotifyEvent;
    FExPanel   : TPanel;
    procedure SetExPanel(APanel : TPanel);
  protected // Fast access for ofen used common features
    property ExPanel : TPanel read FExPanel write SetExPanel;
    property PreviewImage : TImage read FPreviewImage write FPreviewImage;
    property PreviewLabel : TLabel read FPreviewLabel write FPreviewLabel;
  protected
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; virtual;
    procedure BeforeDestruction; virtual;
  published
    property OnCreate  : TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy : TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

procedure Register;

implementation

type
  TSilentPaintPanel = class(TPanel)
  protected
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  end;

procedure TSilentPaintPanel.WMPaint(var Msg: TWMPaint);
begin
  try
    inherited;
  except
    Caption := SInvalidImage;
  end;
end;

constructor TOpenDialogEx.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FOnCreate := nil;
  FOnDestroy := nil;
  FExPanel := nil;
end;

procedure TOpenDialogEx.AfterConstruction;
begin
  if Assigned(FOnCreate) then FOnCreate(Self);
end;

procedure TOpenDialogEx.BeforeDestruction;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
end;

procedure TOpenDialogEx.SetExPanel(APanel : TPanel);
begin
  if not Assigned(FExPanel) then begin
    TSplitter.Create(Self);
  end;
  FExPanel := APanel;
  FExPanel.Align := alLeft;
end;

procedure OpenDialogEx.DoClose;
begin
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
end;



procedure Register;
begin
  RegisterComponents('Dialogues', [TOpenDialogEx]);
end;

end.
