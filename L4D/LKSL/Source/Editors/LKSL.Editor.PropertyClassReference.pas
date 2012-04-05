unit LKSL.Editor.PropertyClassReference;

interface

{$I LKSL.inc}

uses
  {$IFDEF DELPHIXE2}
    System.Classes, System.Rtti, System.SysUtils, VCL.Dialogs,
    Winapi.Windows, Winapi.Messages, System.Variants, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  {$ELSE}
    Classes, Rtti, SysUtils, Dialogs,
    Windows, Messages, Variants, Graphics, Controls, Forms,
  {$ENDIF}
  DesignIntf, DesignEditors,
  LKSL.Editor.PropertyClassReferenceIntf;

type
  TClassRefProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TdlgClassTypeSelector = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dlgClassTypeSelector: TdlgClassTypeSelector;

implementation

{$R *.dfm}

{ TClassRefProperty }

procedure TClassRefProperty.Edit;
begin
  inherited;

end;

function TClassRefProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
