//******************************************************************************
//***                     SCRIPT VCL FUNCTIONS                               ***
//***                                                                        ***
//***        (c) Massimo Magnano 2006                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : Script_DB.pas      (rev. 2.0)
//
//  Description : Access from scripts to Classes declared in "DB.pas"
//
//******************************************************************************
//
//  TComponent <- TDataset
//             <- TField
//  -------------------------------------------------------------
//  TScriptComponent <- TScriptDataset
//                   <- TScriptField
//==============================================================================

unit Script_DB;

interface

uses Classes, DB, TypInfo, Script_Classes;

type
    TScriptDataset = class (TScriptComponent)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;

    public
       function GetArrayPropType(Name :String; index :Variant) :PTypeInfo; override;
       function GetArrayProp(Name :String; index :Variant) :Variant; override;

    function ActiveBuffer: String;
    procedure Append;
    procedure AppendRecord(const Values: array of const);
    function BookmarkValid(Bookmark: TBookmark): Boolean;
    procedure Cancel;
    procedure CheckBrowseMode;
    procedure ClearFields;
    procedure Close;
    function  ControlsDisabled: Boolean;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
    procedure CursorPosChanged;
    procedure Delete;
    procedure DisableControls;
    procedure Edit;
    procedure EnableControls;
    function FieldByName(const FieldName: string): TField;
    function FindField(const FieldName: string): TField;
    function FindFirst: Boolean;
    function FindLast: Boolean;
    function FindNext: Boolean;
    function FindPrior: Boolean;
    procedure First;
    procedure FreeBookmark(Bookmark: TBookmark);
    function GetBookmark: TBookmark;
    function GetCurrentRecord(Buffer: String): Boolean;
    procedure GetDetailDataSets(List: TList);
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList);
    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload;
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; overload;
    procedure GetFieldList(List: TList; const FieldNames: string);
    procedure GetFieldNames(List: TStrings);
    procedure GotoBookmark(Bookmark: TBookmark);
    procedure Insert;
    procedure InsertRecord(const Values: array of const);
    function IsEmpty: Boolean;
    function IsLinkedTo(DataSource: TDataSource): Boolean;
    function IsSequenced: Boolean;
    procedure Last;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant;
    function MoveBy(Distance: Integer): Integer;
    procedure Next;
    procedure Open;
    procedure Post;
    procedure Prior;
    procedure Refresh;
    procedure Resync(Mode: TResyncMode);
    procedure SetFields(const Values: array of const);
    function Translate(Src, Dest: String; ToOem: Boolean): Integer;
    procedure UpdateCursorPos;
    procedure UpdateRecord;
    function UpdateStatus: TUpdateStatus;
    end;

//==============================================================================
    TScriptField = class (TScriptComponent)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;

    public
    procedure AssignValue(const Value: TVarRec);
    procedure Clear;
    procedure FocusControl;
    function GetData(Buffer: Pointer; NativeFormat: Boolean = True): Boolean;
    class function IsBlob: Boolean;
    function IsValidChar(InputChar: Char): Boolean;
    procedure RefreshLookupList;
    procedure SetData(Buffer: Pointer; NativeFormat: Boolean = True);
    procedure SetFieldType(Value: TFieldType);
    procedure Validate(Buffer: Pointer);
    end;

implementation

uses Script_System, SysUtils, Variants;

type
    TDatasetAccess = class(TDataset)
    published
       property AggFields;
       property Bof;
       property Bookmark;
       property CanModify;
       property DataSetField;
       property DataSource;
       property DefaultFields;
       property Designer;
       property Eof;
       property BlockReadSize;
       property FieldCount;
       property FieldDefs;
       property FieldDefList;
       property Fields;
       property FieldList;
       //property FieldValues[const FieldName: string]: Variant
       property Found;
       property IsUniDirectional;
       property Modified;
       property ObjectView;
       property RecordCount;
       property RecNo;
       property RecordSize;
       property SparseArrays;
       property State;
       property Filter;
       property Filtered;
       property FilterOptions;
       property Active;
       property AutoCalcFields;
    end;

//==============================================================================
    TFieldAccess = class(TField)
    published
       property AsBCD;
       property AsBoolean;
       property AsCurrency;
       property AsDateTime;
       property AsSQLTimeStamp;
       property AsFloat;
       property AsInteger;
       property AsString;
       property AsVariant;
       property AttributeSet;
       property Calculated;
       property CanModify;
       property CurValue;
       property DataSet;
       property DataSize;
       property DataType;
       property DisplayName;
       property DisplayText;
       property EditMask;
       property EditMaskPtr;
       property FieldNo;
       property FullName;
       property IsIndexField;
       property IsNull;
       property Lookup;
       property LookupList;
       property NewValue;
       property Offset;
       property OldValue;
       property ParentField;
       property Size;
       property Text;
       //property ValidChars;  Size of published set 'ValidChars' is >4 bytes
       property Value;
    end;

{ TScriptDataset }

function TScriptDataset.GetArrayPropType(Name: String; index: Variant): PTypeInfo;
begin
     Name :=Uppercase(Name);
     Result :=nil;

     if (Name='FIELDVALUES')
     then begin
               if (TDataset(InstanceObj).FieldValues[index]<>NULL)
               then Result :=TypeInfo(Variant);
          end
     else
     Result :=inherited GetArrayPropType(Name, index);
end;

function TScriptDataset.GetArrayProp(Name: String; index: Variant): Variant;
begin
     if (Name='FIELDVALUES')
     then begin
               Result :=TDataset(InstanceObj).FieldValues[index]
          end
     else
     Result := inherited GetArrayProp(name, index)
end;

class function TScriptDataset.GetPublicPropertyAccessClass: TClass;
begin
     Result :=TDatasetAccess;
end;

function TScriptDataset.FindLast: Boolean;
begin
     Result :=TDataset(InstanceObj).FindLast;
end;

function TScriptDataset.FieldByName(const FieldName: string): TField;
begin
     Result :=TDataset(InstanceObj).FieldByName(FieldName);
end;

function TScriptDataset.FindPrior: Boolean;
begin
     Result :=TDataset(InstanceObj).FindPrior;
end;

function TScriptDataset.FindField(const FieldName: string): TField;
begin
     Result :=TDataset(InstanceObj).FindField(FieldName);
end;

function TScriptDataset.FindFirst: Boolean;
begin
     Result :=TDataset(InstanceObj).FindFirst;
end;

procedure TScriptDataset.First;
begin
     TDataset(InstanceObj).First;
end;

function TScriptDataset.FindNext: Boolean;
begin
     Result :=TDataset(InstanceObj).FindNext;
end;

function TScriptDataset.IsLinkedTo(DataSource: TDataSource): Boolean;
begin
     Result :=TDataset(InstanceObj).IsLinkedTo(DataSource);
end;

procedure TScriptDataset.ClearFields;
begin
    TDataset(InstanceObj).ClearFields;
end;

procedure TScriptDataset.CursorPosChanged;
begin
     TDataset(InstanceObj).CursorPosChanged;
end;

function TScriptDataset.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
     Result :=TDataset(InstanceObj).Lookup(KeyFields, KeyValues, ResultFields);
end;

procedure TScriptDataset.Last;
begin
     TDataset(InstanceObj).Last;
end;

function TScriptDataset.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
     Result :=TDataset(InstanceObj).Locate(KeyFields, KeyValues, Options);
end;

procedure TScriptDataset.SetFields(const Values: array of const);
begin
     TDataset(InstanceObj).SetFields(Values);
end;

procedure TScriptDataset.CheckBrowseMode;
begin
     TDataset(InstanceObj).CheckBrowseMode;
end;

function TScriptDataset.UpdateStatus: TUpdateStatus;
begin
     Result :=TDataset(InstanceObj).UpdateStatus;
end;

function TScriptDataset.MoveBy(Distance: Integer): Integer;
begin
     Result :=TDataset(InstanceObj).MoveBy(Distance);
end;

function TScriptDataset.IsSequenced: Boolean;
begin
     Result :=TDataset(InstanceObj).IsSequenced;
end;

procedure TScriptDataset.Prior;
begin
     TDataset(InstanceObj).Prior;
end;

procedure TScriptDataset.UpdateRecord;
begin
     TDataset(InstanceObj).UpdateRecord;
end;

procedure TScriptDataset.Refresh;
begin
     TDataset(InstanceObj).Refresh;
end;

procedure TScriptDataset.Open;
begin
     TDataset(InstanceObj).Open;
end;

procedure TScriptDataset.DisableControls;
begin
     TDataset(InstanceObj).DisableControls;
end;

procedure TScriptDataset.AppendRecord(const Values: array of const);
begin
     TDataset(InstanceObj).AppendRecord(Values);
end;

procedure TScriptDataset.Cancel;
begin
     TDataset(InstanceObj).Cancel;
end;

procedure TScriptDataset.Post;
begin
     TDataset(InstanceObj).Post;
end;

procedure TScriptDataset.InsertRecord(const Values: array of const);
begin
     TDataset(InstanceObj).InsertRecord(Values);
end;

function TScriptDataset.IsEmpty: Boolean;
begin
     Result :=TDataset(InstanceObj).IsEmpty;
end;

procedure TScriptDataset.Close;
begin
     TDataset(InstanceObj).Close;
end;

procedure TScriptDataset.EnableControls;
begin
     TDataset(InstanceObj).EnableControls;
end;

procedure TScriptDataset.Delete;
begin
     TDataset(InstanceObj).Delete;
end;

procedure TScriptDataset.Resync(Mode: TResyncMode);
begin
     TDataset(InstanceObj).Resync(Mode);
end;

procedure TScriptDataset.Edit;
begin
     TDataset(InstanceObj).Edit;
end;

procedure TScriptDataset.Append;
begin
     TDataset(InstanceObj).Append;
end;

function TScriptDataset.ControlsDisabled: Boolean;
begin
     Result :=TDataset(InstanceObj).ControlsDisabled;
end;

procedure TScriptDataset.UpdateCursorPos;
begin
     TDataset(InstanceObj).UpdateCursorPos;
end;

procedure TScriptDataset.Next;
begin
     TDataset(InstanceObj).Next;
end;

procedure TScriptDataset.Insert;
begin
     TDataset(InstanceObj).Insert;
end;

procedure TScriptDataset.GetFieldList(List: TList; const FieldNames: string);
begin
     TDataset(InstanceObj).GetFieldList(List, FieldNames);
end;

function TScriptDataset.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
     Result :=TDataset(InstanceObj).GetFieldData(Field, Buffer, NativeFormat);
end;

function TScriptDataset.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
begin
     Result :=TDataset(InstanceObj).GetFieldData(FieldNo, Buffer);
end;

function TScriptDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
     Result :=TDataset(InstanceObj).GetFieldData(Field, Buffer);
end;

function TScriptDataset.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
     Result :=TDataset(InstanceObj).CreateBlobStream(Field, Mode);
end;

procedure TScriptDataset.GotoBookmark(Bookmark: TBookmark);
begin
     TDataset(InstanceObj).GotoBookmark(Bookmark);
end;

function TScriptDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
     Result :=TDataset(InstanceObj).CompareBookmarks(Bookmark1, Bookmark2);
end;

function TScriptDataset.GetBookmark: TBookmark;
begin
     Result :=TDataset(InstanceObj).GetBookmark;
end;

function TScriptDataset.GetCurrentRecord(Buffer: String): Boolean;
begin
     Result :=TDataset(InstanceObj).GetCurrentRecord(PChar(Buffer));
end;

procedure TScriptDataset.GetDetailLinkFields(MasterFields, DetailFields: TList);
begin
     TDataset(InstanceObj).GetDetailLinkFields(MasterFields, DetailFields);
end;

function TScriptDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
     Result :=TDataset(InstanceObj).BookmarkValid(Bookmark);
end;

function TScriptDataset.Translate(Src, Dest: String; ToOem: Boolean): Integer;
begin
     Result :=TDataset(InstanceObj).Translate(PChar(Src), PChar(Dest), ToOem);
end;

procedure TScriptDataset.FreeBookmark(Bookmark: TBookmark);
begin
     TDataset(InstanceObj).FreeBookmark(Bookmark);
end;

function TScriptDataset.GetBlobFieldData(FieldNo: Integer;
  var Buffer: TBlobByteData): Integer;
begin
     Result :=TDataset(InstanceObj).GetBlobFieldData(FieldNo, Buffer);
end;

procedure TScriptDataset.GetDetailDataSets(List: TList);
begin
     TDataset(InstanceObj).GetDetailDataSets(List);
end;

function TScriptDataset.ActiveBuffer: String;
begin
     Result :=TDataset(InstanceObj).ActiveBuffer;
end;

procedure TScriptDataset.GetFieldNames(List: TStrings);
begin
     TDataset(InstanceObj).GetFieldNames(List);
end;

{ TScriptField }

class function TScriptField.GetPublicPropertyAccessClass: TClass;
begin
     Result :=TFieldAccess;
end;

procedure TScriptField.AssignValue(const Value: TVarRec);
begin
     TField(InstanceObj).AssignValue(Value);
end;

function TScriptField.GetData(Buffer: Pointer; NativeFormat: Boolean): Boolean;
begin
     Result :=TField(InstanceObj).GetData(Buffer, NativeFormat);
end;

procedure TScriptField.FocusControl;
begin
     TField(InstanceObj).FocusControl;
end;

procedure TScriptField.Clear;
begin
     TField(InstanceObj).Clear;
end;

procedure TScriptField.SetData(Buffer: Pointer; NativeFormat: Boolean);
begin
     TField(InstanceObj).SetData(Buffer, NativeFormat);
end;

procedure TScriptField.SetFieldType(Value: TFieldType);
begin
     TField(InstanceObj).SetFieldType(Value);
end;

procedure TScriptField.Validate(Buffer: Pointer);
begin
     TField(InstanceObj).Validate(Buffer);
end;

procedure TScriptField.RefreshLookupList;
begin
     TField(InstanceObj).RefreshLookupList;
end;

class function TScriptField.IsBlob: Boolean;
begin
     Result := False;
end;

function TScriptField.IsValidChar(InputChar: Char): Boolean;
begin
     Result :=TField(InstanceObj).IsValidChar(InputChar);
end;

initialization
   Script_System.RegisterClass(TDataset, TScriptDataset);
   Script_System.RegisterClass(TField, TScriptField);

end.
