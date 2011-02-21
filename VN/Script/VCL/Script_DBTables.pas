//******************************************************************************
//***                     SCRIPT VCL FUNCTIONS                               ***
//***                                                                        ***
//***        (c) Massimo Magnano 2006                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : Script_DBTables.pas      (rev. 2.0)
//
//  Description : Access from scripts to Classes declared in "DBTables.pas"
//
//******************************************************************************
//
//  TDataset <- TBDEDataSet <- TDBDataSet <- TTable
//                                        <- TQuery
//  -------------------------------------------------------------
//  TScriptDataset <- TScriptBDEDataSet <- TScriptDBDataSet <- TScriptTable
//                                                          <- TScriptQuery
//==============================================================================

unit Script_DBTables;

interface

uses TypInfo, Classes, DB, BDE, DBTables, Script_DB;

type
//==============================================================================
    TScriptBDEDataSet = class (TScriptDataset)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;

    public
(*       function GetArrayPropType(Name :String; index :Variant) :PTypeInfo; override;
       function GetArrayProp(Name :String; index :Variant) :Variant; override;
       function GetElementType(Name :String) :PTypeInfo; override;
       function GetElement(Name :String) :Variant; override;
*)
    procedure ApplyUpdates;
    procedure CancelUpdates;
    procedure CommitUpdates;
    function ConstraintCallBack(Req: DsInfoReq; var ADataSources: DataSources): DBIResult;
    function ConstraintsDisabled: Boolean;
    procedure DisableConstraints;
    procedure EnableConstraints;
    procedure FetchAll;
    procedure FlushBuffers;
    procedure GetIndexInfo;
    procedure RevertRecord;
    end;

//==============================================================================
    TScriptDBDataSet = class (TScriptBDEDataSet)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;

    public
(*       function GetArrayPropType(Name :String; index :Variant) :PTypeInfo; override;
       function GetArrayProp(Name :String; index :Variant) :Variant; override;
       function GetElementType(Name :String) :PTypeInfo; override;
       function GetElement(Name :String) :Variant; override;
*)
    function CheckOpen(Status: DBIResult): Boolean;
    procedure CloseDatabase(Database: TDatabase);
    function OpenDatabase: TDatabase;
    end;


//==============================================================================
    TScriptTable = class (TScriptDBDataSet)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;

    public
       function GetArrayPropType(Name :String; index :Variant) :PTypeInfo; override;
       function GetArrayProp(Name :String; index :Variant) :Variant; override;

    function BatchMove(ASource: TBDEDataSet; AMode: TBatchMode): Longint;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions;
      const DescFields: string = '');
    procedure ApplyRange;
    procedure CancelRange;
    procedure CloseIndexFile(const IndexFileName: string);
    procedure CreateTable;
    procedure DeleteIndex(const Name: string);
    procedure DeleteTable;
    procedure EditKey;
    procedure EditRangeEnd;
    procedure EditRangeStart;
    procedure EmptyTable;
    function FindKey(const KeyValues: array of const): Boolean;
    procedure FindNearest(const KeyValues: array of const);
    procedure GetIndexNames(List: TStrings);
    procedure GotoCurrent(Table: TTable);
    function GotoKey: Boolean;
    procedure GotoNearest;
    procedure LockTable(LockType: TLockType);
    procedure OpenIndexFile(const IndexName: string);
    procedure RenameTable(const NewTableName: string);
    procedure SetKey;
    procedure SetRange(const StartValues, EndValues: array of const);
    procedure SetRangeEnd;
    procedure SetRangeStart;
    procedure UnlockTable(LockType: TLockType);
    end;

//==============================================================================
    TScriptQuery = class (TScriptDBDataSet)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;

    public
(*       function GetArrayPropType(Name :String; index :Variant) :PTypeInfo; override;
       function GetArrayProp(Name :String; index :Variant) :Variant; override;
       function GetElementType(Name :String) :PTypeInfo; override;
       function GetElement(Name :String) :Variant; override;
*)
    procedure ExecSQL;
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure UnPrepare;
    end;



implementation

uses Variants, SysUtils, Script_System;

type
//==============================================================================
    TBDEDataSetAccess = class(TBDEDataSet)
    published
       property CacheBlobs;
       property ExpIndex;
       //property Handle;  Pointer
       property KeySize;
       //property Locale;  Pointer
       property UpdateObject;
       property UpdatesPending;
       property UpdateRecordTypes;
    end;

//==============================================================================
    TDBDataSetAccess = class(TDBDataSet)
    published
       property Database;
       //property DBHandle; Pointer
       //property DBLocale; Pointer
       property DBSession;
       //property Handle; Pointer
    end;

//==============================================================================
    TTableAccess = class(TTable)
    published
       property Exists;
       property IndexFieldCount;
       //property IndexFields[Index: Integer]: TField;
       property KeyExclusive;
       property KeyFieldCount;
       property TableLevel;
    end;

//==============================================================================
    TQueryAccess = class(TQuery)
    published
       property Prepared;
       property ParamCount;
       property Local;
       //property StmtHandle; Pointer
       property Text;
       property RowsAffected;
       //property SQLBinary; Pointer
    end;

{ TScriptBDEDataSet }

class function TScriptBDEDataSet.GetPublicPropertyAccessClass: TClass;
begin
     Result :=TBDEDataSetAccess;
end;

procedure TScriptBDEDataSet.CancelUpdates;
begin
     TBDEDataset(InstanceObj).CancelUpdates;
end;

procedure TScriptBDEDataSet.ApplyUpdates;
begin
     TBDEDataset(InstanceObj).ApplyUpdates;
end;

procedure TScriptBDEDataSet.CommitUpdates;
begin
     TBDEDataset(InstanceObj).CommitUpdates;
end;

procedure TScriptBDEDataSet.DisableConstraints;
begin
     TBDEDataset(InstanceObj).DisableConstraints;
end;

procedure TScriptBDEDataSet.FetchAll;
begin
     TBDEDataset(InstanceObj).FetchAll;
end;

procedure TScriptBDEDataSet.EnableConstraints;
begin
     TBDEDataset(InstanceObj).EnableConstraints;
end;

function TScriptBDEDataSet.ConstraintsDisabled: Boolean;
begin
     Result :=TBDEDataset(InstanceObj).ConstraintsDisabled;
end;

procedure TScriptBDEDataSet.GetIndexInfo;
begin
     TBDEDataset(InstanceObj).GetIndexInfo;
end;

function TScriptBDEDataSet.ConstraintCallBack(Req: DsInfoReq;
  var ADataSources: DataSources): DBIResult;
begin
     Result :=TBDEDataset(InstanceObj).ConstraintCallBack(Req, ADataSources);
end;

procedure TScriptBDEDataSet.FlushBuffers;
begin
     TBDEDataset(InstanceObj).FlushBuffers;
end;

procedure TScriptBDEDataSet.RevertRecord;
begin
     TBDEDataset(InstanceObj).RevertRecord;
end;

{ TScriptDBDataSet }

class function TScriptDBDataSet.GetPublicPropertyAccessClass: TClass;
begin
     Result :=TDBDataSetAccess;
end;

procedure TScriptDBDataSet.CloseDatabase(Database: TDatabase);
begin
     TDBDataSet(InstanceObj).CloseDatabase(Database);
end;

function TScriptDBDataSet.CheckOpen(Status: DBIResult): Boolean;
begin
     Result :=TDBDataSet(InstanceObj).CheckOpen(Status);
end;

function TScriptDBDataSet.OpenDatabase: TDatabase;
begin
     Result :=TDBDataSet(InstanceObj).OpenDatabase;
end;

{ TScriptTable }

function TScriptTable.GetArrayPropType(Name: String; index: Variant): PTypeInfo;
begin
     Name :=Uppercase(Name);
     Result :=nil;

     if (Name='INDEXFIELDS')
     then begin
               if (TTable(InstanceObj).IndexFields[index]<>nil)
               then Result :=TypeInfo(TField);
          end
     else
     Result :=inherited GetArrayPropType(Name, index);
end;

function TScriptTable.GetArrayProp(Name: String; index: Variant): Variant;
begin
     if (Name='INDEXFIELDS')
     then begin
               if (TTable(InstanceObj).IndexFields[index]<>nil)
               then Result :=Integer(TTable(InstanceObj).IndexFields[index])
               else Result :=NULL;
          end
     else
     Result := inherited GetArrayProp(name, index)
end;

class function TScriptTable.GetPublicPropertyAccessClass: TClass;
begin
     Result :=TTableAccess;
end;

procedure TScriptTable.SetRange(const StartValues, EndValues: array of const);
begin
     TTable(InstanceObj).SetRange(StartValues, EndValues);
end;

procedure TScriptTable.GetIndexNames(List: TStrings);
begin
     TTable(InstanceObj).GetIndexNames(List);
end;

procedure TScriptTable.SetKey;
begin
     TTable(InstanceObj).SetKey;
end;

procedure TScriptTable.CreateTable;
begin
     TTable(InstanceObj).CreateTable;
end;

procedure TScriptTable.GotoNearest;
begin
     TTable(InstanceObj).GotoNearest;
end;

procedure TScriptTable.RenameTable(const NewTableName: string);
begin
     TTable(InstanceObj).RenameTable(NewTableName);
end;

procedure TScriptTable.CloseIndexFile(const IndexFileName: string);
begin
     TTable(InstanceObj).CloseIndexFile(IndexFileName);
end;

procedure TScriptTable.DeleteIndex(const Name: string);
begin
     TTable(InstanceObj).DeleteIndex(Name);
end;

function TScriptTable.BatchMove(ASource: TBDEDataSet; AMode: TBatchMode): Longint;
begin
     Result :=TTable(InstanceObj).BatchMove(ASource, AMode);
end;

procedure TScriptTable.EditRangeStart;
begin
     TTable(InstanceObj).EditRangeStart;
end;

procedure TScriptTable.CancelRange;
begin
     TTable(InstanceObj).CancelRange;
end;

function TScriptTable.GotoKey: Boolean;
begin
     Result :=TTable(InstanceObj).GotoKey;
end;

procedure TScriptTable.ApplyRange;
begin
     TTable(InstanceObj).ApplyRange;
end;

procedure TScriptTable.LockTable(LockType: TLockType);
begin
     TTable(InstanceObj).LockTable(LockType);
end;

procedure TScriptTable.FindNearest(const KeyValues: array of const);
begin
     TTable(InstanceObj).FindNearest(KeyValues);
end;

procedure TScriptTable.UnlockTable(LockType: TLockType);
begin
     TTable(InstanceObj).UnlockTable(LockType);
end;

procedure TScriptTable.GotoCurrent(Table: TTable);
begin
     TTable(InstanceObj).GotoCurrent(Table);
end;

procedure TScriptTable.SetRangeStart;
begin
     TTable(InstanceObj).SetRangeStart;
end;

procedure TScriptTable.AddIndex(const Name, Fields: string; Options: TIndexOptions;
  const DescFields: string);
begin
     TTable(InstanceObj).AddIndex(Name, Fields, Options, DescFields);
end;

procedure TScriptTable.EditRangeEnd;
begin
     TTable(InstanceObj).EditRangeEnd;
end;

procedure TScriptTable.EditKey;
begin
     TTable(InstanceObj).EditKey;
end;

procedure TScriptTable.DeleteTable;
begin
     TTable(InstanceObj).DeleteTable;
end;

function TScriptTable.FindKey(const KeyValues: array of const): Boolean;
begin
     Result :=TTable(InstanceObj).FindKey(KeyValues);
end;

procedure TScriptTable.EmptyTable;
begin
     TTable(InstanceObj).EmptyTable;
end;

procedure TScriptTable.SetRangeEnd;
begin
     TTable(InstanceObj).SetRangeEnd;
end;

procedure TScriptTable.OpenIndexFile(const IndexName: string);
begin
     TTable(InstanceObj).OpenIndexFile(IndexName);
end;

{ TScriptQuery }

class function TScriptQuery.GetPublicPropertyAccessClass: TClass;
begin
     Result :=TQueryAccess;
end;

procedure TScriptQuery.ExecSQL;
begin
     TQuery(InstanceObj).ExecSQL;
end;

function TScriptQuery.ParamByName(const Value: string): TParam;
begin
     Result :=TQuery(InstanceObj).ParamByName(Value);
end;

procedure TScriptQuery.Prepare;
begin
     TQuery(InstanceObj).Prepare;
end;

procedure TScriptQuery.UnPrepare;
begin
     TQuery(InstanceObj).UnPrepare;
end;

initialization
   Script_System.RegisterClass(TBDEDataSet, TScriptBDEDataSet);
   Script_System.RegisterClass(TDBDataSet, TScriptDBDataSet);
   Script_System.RegisterClass(TTable, TScriptTable);
   Script_System.RegisterClass(TQuery, TScriptQuery);


end.
