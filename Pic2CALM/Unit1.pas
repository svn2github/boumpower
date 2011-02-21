unit Unit1;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TEditSymbolDlg = class(TForm)
    ListBox1: TListBox;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure ListBox1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
    ListModified : boolean;
  public
    { Public declarations }
    function EditList(List : TStringList) : boolean;
  end;

var
  EditSymbolDlg: TEditSymbolDlg;

implementation

{$R *.DFM}

procedure TEditSymbolDlg.ListBox1Click(Sender: TObject);
Var
  S : string;
begin
  S := ListBox1.Items[ListBox1.ItemIndex];
  Delete(S, 1, Pos('=', S));
  Edit1.Text := S;
  Edit1.Enabled := True;
  BitBtn1.Enabled := false;
end;

procedure TEditSymbolDlg.Edit1Change(Sender: TObject);
begin
  if Edit1.Modified then BitBtn1.Enabled := True;
end;

procedure TEditSymbolDlg.BitBtn2Click(Sender: TObject);
begin
  Close;
end;

procedure TEditSymbolDlg.BitBtn1Click(Sender: TObject);
Var
  S : string;
begin
  S := ListBox1.Items[ListBox1.ItemIndex];
  Delete(S, Pos('=', S)+1, 255);
  ListBox1.Items[ListBox1.ItemIndex] := S+Edit1.Text;
  BitBtn1.Enabled := false;
  ListModified := True;
end;

function TEditSymbolDlg.EditList(List : TStringList) : boolean;
begin
  ListModified := false;
  List.Sort;
  ListBox1.Clear;
  ListBox1.Items := List;
  ShowModal;
  List.Clear;
  List.AddStrings(ListBox1.Items);
  Result := ListModified;
end;


end.
