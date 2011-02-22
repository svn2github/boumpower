unit Internal;

interface

uses
 Windows, QTypes, SysUtils, Classes, IniFiles;

const
  TOCResourceName = 'INTERNAL.TOC';
  faResourceFile  = 128;

type
// share a liste of internal(resource RCDATA) files
// The Objects property of Dest strings in GetDirectoryInfo hold a PFileInfo with Attr field faResourceFile set
TCustomFAT = class(TComponent)
  private
    FTOC : TMemIniFile; // Table of Content for Internal resources
    FAllocatedFileInfos : TStringList; // Hold allocated FileInfo
  protected
  public
    constructor Create(AOwner: TComponent; const TOCResourceName : string); overload;
    destructor  Destroy; override;
    function GetDirectoryInfo(const PseudoPath : string; Dest : TStrings; const AddFileInfos : boolean) : integer;
  end;

var
  InternalFAT : TCustomFAT;

function ReadResource(const ResourceName : string; Dest : TStrings) : boolean;

implementation

{$R *.res}

constructor TCustomFAT.Create(AOwner: TComponent; const TOCResourceName : string);
var
  S      : TStringList;
begin
  inherited Create(AOwner);
  FAllocatedFileInfos := TStringList.Create;
  FAllocatedFileInfos.Sorted := true;
  FTOC := TMemIniFile.Create('');
  S := TStringList.Create;
  if ReadResource(TOCResourceName, S) then FTOC.SetStrings(S);
  S.Free;
end;

destructor  TCustomFAT.Destroy;
begin
  while FAllocatedFileInfos.Count>0 do begin
    Dispose(PFileInfo(FAllocatedFileInfos.Objects[0]));
    FAllocatedFileInfos.Delete(0);
  end;
  FAllocatedFileInfos.Free;
  FTOC.Free;
end;

function TCustomFAT.GetDirectoryInfo(const PseudoPath : string; Dest : TStrings; const AddFileInfos : boolean) : integer;
var
  PInfo    : PFileInfo;
  i        : integer;
  item     : integer;
  Path     : string;

begin
  Path := 'Directory.'+PseudoPath;
  result := FTOC.ReadInteger(Path, 'NumFiles', 0);
  i := 0;
  while i<result do begin
    Path := 'Directory.'+PseudoPath+Format('.File%u', [i]);
    if FAllocatedFileInfos.Find(Path, item) then begin
      PInfo := PFileInfo(FAllocatedFileInfos.Objects[item])
    end else begin
      New(PInfo);
      with PInfo^.SR do begin
        Name := FTOC.ReadString(Path, 'Name', 'NoName');
        Time := DateTimeToFileDate(FTOC.ReadDateTime(Path, 'Time', 0));
        Size := FTOC.ReadInteger(Path, 'Size', -1);        Attr := FTOC.ReadInteger(Path, 'Attr', faReadOnly + faResourceFile);      end;      if FindResource(HInstance, PChar(PInfo^.SR.Name), RT_RCDATA)<>0 then
        PInfo^.Desc := FTOC.ReadString(Path, 'Desc', PInfo^.SR.Name)
      else
        PInfo^.Desc := 'RCDADA not found'+PInfo^.SR.Name;
      FAllocatedFileInfos.AddObject(PInfo^.Desc, TObject(PInfo));
    end;
    if AddFileInfos then
      Dest.AddObject(PInfo^.Desc, TObject(PInfo))
    else
      Dest.Add(PInfo^.Desc);
    Inc(i);
  end;
end;

function ReadResource(const ResourceName : string; Dest : TStrings) : boolean;
var
  Stream : TResourceStream;
begin
  if FindResource(HInstance, PChar(ResourceName), RT_RCDATA)<>0 then begin
    Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    Dest.LoadFromStream(Stream);
    Stream.Free;
    result := true;
  end else
    result := false;
end;

initialization

InternalFAT := TCustomFAT.Create(nil, TOCResourceName);

finalization

InternalFAT.Free;

end.
