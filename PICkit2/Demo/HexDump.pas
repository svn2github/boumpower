unit HexDump;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms;

type
  THexDumpDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
  end;

var
  HexDumpDialog: THexDumpDialog;

implementation

{$R *.DFM}

end.
