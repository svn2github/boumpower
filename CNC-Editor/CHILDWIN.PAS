unit CHILDWIN;

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls;

type
  TMDIChild = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { déclarations privées }
  public
    { déclarations publiques }
  end;

implementation

{$R *.dfm}

procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
