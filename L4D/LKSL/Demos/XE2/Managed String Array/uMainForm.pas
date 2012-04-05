unit uMainForm;

interface

{$I LKSL.inc}

uses
  {$IFDEF SCOPED}
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin
  {$ELSE}
    Windows, Messages, SysUtils, Variants, Classes, Graphics,
    Controls, Forms, Dialogs, StdCtrls, Spin
  {$ENDIF}, LKSL.Managed.Arrays;

type
  {
    We inherit "TTestObject" from our TStringArrayObject base class!
    We can then provide custom data fields (as well as any custom methods).

    This is the SUBJECT our Manager manages!
  }
  TTestObject = class(TStringArrayObject)
  private
    FValue: String; // Our custom data field
  published
    property Value: String read FValue; // A property accessor for the custom data field
  end;

  {
    We inherit "TTestManager" from our TStringArrayManager base class!
    We set it up in a specific way to make use of the base manager's optimized
    sorted insertion, search, and memory management features.
  }
  TTestManager = class(TStringArrayManager)
  private
    function GetTestObjectByName(const AName: String): TTestObject; // Must be provided to return Type-cast objects by Name
    function GetTestObjectByIndex(const AIndex: Integer): TTestObject; // Must be provided to return Type-cast objects by Index
  public
    constructor Create; override; // Must be provided to provide the Manager base class with the Object Class Type
    function AddTestObject(const AName: String; const AValue: String): TTestObject; // We need this in order to add new objects to the array!
    property TestObjects[const AName: String]: TTestObject read GetTestObjectByName; default; // An Array Property accessor
    property TestObjects[const AIndex: Integer]: TTestObject read GetTestObjectByIndex; default; // An Array Property accessor
  end;

  TForm1 = class(TForm)
    memValues: TMemo;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    edKey: TEdit;
    edValue: TEdit;
    Label2: TLabel;
    GroupBox3: TGroupBox;
    edSearchKey: TEdit;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    odCSV: TOpenDialog;
    sdCSV: TSaveDialog;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    sePasses: TSpinEdit;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    FArrayManager: TTestManager;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TTestManager }

function TTestManager.AddTestObject(const AName, AValue: String): TTestObject;
begin
  Result := TTestObject(AddObject(AName)); // Returns the object, correctly typed!
  Result.FValue := AValue; // Sets our custom field value
end;

constructor TTestManager.Create;
begin
  inherited;
  FObjectType := TTestObject; // Tells the base Array Manager what the Object Type is (essential for Memory Allocation)
end;

function TTestManager.GetTestObjectByIndex(const AIndex: Integer): TTestObject;
begin
  Result := TTestObject(GetObjectByIndex(AIndex)); // Returns the indexed object, correctly typed!
end;

function TTestManager.GetTestObjectByName(const AName: String): TTestObject;
begin
  Result := TTestObject(GetObjectByName(AName)); // Returns the named object (if it exists), correctly typed!
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  DISPLAY_STRING = 'The value of "%s" is "%s" (Found in just %d ms)';
var
  LStartTime: Cardinal;
  FTestObject: TTestObject;
begin
  LStartTime := GetTickCount;
  FTestObject := FArrayManager.TestObjects[edSearchKey.Text];
  if FTestObject <> nil then
    ShowMessage(Format(DISPLAY_STRING, [FTestObject.Name, FTestObject.Value, GetTickCount - LStartTime]))
  else
    ShowMessage('No entry found for the given key');
  edSearchKey.Clear;
  edSearchKey.SetFocus;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FArrayManager.AddTestObject(edKey.Text, edValue.Text);
  edKey.Clear;
  edValue.Clear;
  edKey.SetFocus;
end;

procedure TForm1.Button3Click(Sender: TObject);
const
  LIST_STRING = '%s = %s';
var
  I: Integer;
begin
  memValues.Lines.BeginUpdate;
  memValues.Clear;
  for I := 0 to FArrayManager.Count - 1 do
    memValues.Lines.Add(Format(LIST_STRING, [FArrayManager.TestObjects[I].Name, FArrayManager.TestObjects[I].Value]));
  memValues.Lines.EndUpdate;
end;

procedure TForm1.Button4Click(Sender: TObject);
const
  LIST_STRING = '%s,%s';
var
  LStrings: TStringList;
  I: Integer;
begin
  LStrings := TStringList.Create;
  try
    if odCSV.Execute then
    begin
      LStrings.LoadFromFile(odCSV.FileName);
      for I := 0 to LStrings.Count - 1 do
      begin
        FArrayManager.AddTestObject(Copy(LStrings[I], 1, Pos(',', LStrings[I]) - 1), Copy(LStrings[I], Pos(',', LStrings[I]) + 1, Length(LStrings[I])));
      end;
    end;
  finally
    LStrings.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
const
  LIST_STRING = '%s,%s';
var
  LStrings: TStringList;
  I: Integer;
begin
  LStrings := TStringList.Create;
  try
    for I := 0 to FArrayManager.Count - 1 do
      LStrings.Add(Format(LIST_STRING, [FArrayManager.TestObjects[I].Name, FArrayManager.TestObjects[I].Value]));
    if sdCSV.Execute then
      LStrings.SaveToFile(sdCSV.FileName + '.csv');
  finally
    LStrings.Free;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FArrayManager.Clear; // Erases all Key/Value pairs in the Array
  memValues.Lines.BeginUpdate;
  memValues.Clear;
  memValues.Lines.EndUpdate;  
end;

procedure TForm1.Button7Click(Sender: TObject);
const
  DISPLAY_STRING = 'The value of "%s" is "%s" (Found in %d ms)';
var
  I: Integer;
  LStartTime: Cardinal;
  FTestObject: TTestObject;
begin
  LStartTime := GetTickCount;
  FTestObject := nil;
  for I := 0 to FArrayManager.Count - 1 do
    if FArrayManager.TestObjects[I].Name = edSearchKey.Text then
    begin    
      FTestObject := FArrayManager.TestObjects[I];
      Break;
    end;
  if FTestObject <> nil then
    ShowMessage(Format(DISPLAY_STRING, [FTestObject.Name, FTestObject.Value, GetTickCount - LStartTime]))
  else
    ShowMessage('No entry found for the given key');
  edSearchKey.Clear;
  edSearchKey.SetFocus;
end;

procedure TForm1.Button8Click(Sender: TObject);
const
  RESULT_STRING = '%d Passes - Iterator: %dms, Optimized Array Manager: %dms (Difference: %dms)';
var
  LManagerStart, LIteratorStart: Cardinal;
  LPass, I: Integer;
  FTestObject: TTestObject;
begin
  LManagerStart := GetTickCount;
  for LPass := 1 to sePasses.Value do
  begin
    FTestObject := FArrayManager.TestObjects[edSearchKey.Text];      
  end;
  LManagerStart := GetTickCount - LManagerStart;

  LIteratorStart := GetTickCount;
  for LPass := 1 to sePasses.Value do
  begin
    for I := 0 to FArrayManager.Count - 1 do
    if FArrayManager.TestObjects[I].Name = edSearchKey.Text then
    begin    
      FTestObject := FArrayManager.TestObjects[I];
      Break;
    end;        
  end;
  LIteratorStart := GetTickCount - LIteratorStart;

  ShowMessage(Format(RESULT_STRING, [sePasses.Value, LIteratorStart, LManagerStart, LIteratorStart - LManagerStart]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FArrayManager := TTestManager.Create; // Instanciates the Test Manager
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FArrayManager.Free; // Frees the Test Manager (and all objects contained therein... absolutely leak-free)
end;

end.
