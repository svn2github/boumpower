unit LKSL.Editor.Register;

interface

{$I LKSL.inc}

uses
  {$IFDEF DELPHIXE2}
    System.Classes,
  {$ELSE}
    Classes,
  {$ENDIF}
  DesignIntf, DesignEditors,
  LKSL.Editor.PropertyClassReferenceIntf;

procedure Register;

implementation

uses LKSL.Editor.PropertyClassReference;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(IClassRefProperty), nil, '', TClassRefProperty);
end;

end.
