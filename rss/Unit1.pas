unit Unit1;

interface

uses
  Windows, Messages, SysUtils, DateUtils, Variants, Types, Classes, Graphics, Controls, Forms,
  Math, Dialogs, StdCtrls, Buttons, OleCtrls, SHDocVw, ComCtrls, JvComponentBase,
  JvHtmlParser, CheckLst, JvSimpleXml, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, HTTPApp, ExtCtrls, Menus;

const
  ScriptURL = 'http://www.boumpower.ch/l3/rss/rss.php';

const
  WM_ScanState = WM_APP+1;

type
  TScanState = (
    ssIdle,
    ssInit, // lit la table des channels
    ssOpenChannel,  // ouvre le channel
    ssReadChannel,  // lit la liste des fils racine du channel
    ssCloseChannel, // ferme le channel
    ssOpenEntry,    // ouvre en fil de discussion
    ssReadEntry,    // lit le fil
    ssCloseEntry,
    ssTerminate);

type
  TFeedEntry = class(TObject)
    guid        : string;  // numéro de la ligne
    title       : string;  // titre
    description : string;  // description
    author      : string;  // email de l'auteur
    pubDate     : TDateTime;  // date de publication
  end;

type
  TForm1 = class(TForm)
    WebBrowser: TWebBrowser;
    StatusBar: TStatusBar;
    Fils: TCheckListBox;
    HTMLParser: TJvHTMLParser;
    FeedSettings: TJvSimpleXML;
    HTTP: TIdHTTP;
    Log: TRichEdit;
    Timer: TTimer;
    MainMenu: TMainMenu;
    Fichier1: TMenuItem;
    Executer1: TMenuItem;
    Quiter1: TMenuItem;
    procedure WebBrowserDocumentComplete(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure WebBrowserNavigateComplete2(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure FeedRootFound(Sender: TObject; Key, Results,
      OriginalLine: String);
    procedure FeedEntryFound(Sender: TObject; Key, Results,
      OriginalLine: String);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Executer1Click(Sender: TObject);
    procedure Quiter1Click(Sender: TObject);
  private
    FScanState   : TScanState;
    FCurDispatch : IDispatch; // pour tester la fin du chargement de page
    FLastBuild   : TDateTime;
    FNewBuild    : TDateTime;
    FCurFeed     : integer;
    FCurChannel  : integer;
    FCounterDIV  : integer; // compte les <DIV>
    FCurGuid     : string;
    FCurTitle    : string;
    FCurAuthor   : string;
    FCurDate     : TDateTime;
    FTTL         : integer;
    procedure SetScanState(AState : TScanState; Msg : string; const Post : Boolean = true);
  protected
    procedure WMScanState(var Message: TMessage); message WM_ScanState;
    procedure AddHistoric(Msg : string; Style : TFontStyles = []; Color : TColor = clDefault);
  public
    procedure Execute;
    procedure ScanInit;
    procedure ScanOpenChannel;
    procedure ScanReadChannel;
    procedure ScanCloseChannel;
    procedure ScanOpenEntry;
    procedure ScanReadEntry;
    procedure ScanCloseEntry;
    procedure ScanTerminate;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  CRLF = #13#10;


procedure TForm1.FormCreate(Sender: TObject);
begin
  Log.Align := alClient;
  FScanState := ssIdle;
  Execute;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
  Execute;
end;


procedure TForm1.SetScanState(AState : TScanState; Msg : string; const Post : Boolean = true);
begin
  FScanState := AState;
  StatusBar.SimpleText := Msg;
  if Post then PostMessage(Self.WindowHandle, WM_ScanState, 0, 0);
end;

procedure TForm1.WMScanState(var Message: TMessage);
begin
  Case FScanState of
    ssInit : ScanInit;
    ssOpenChannel  : ScanOpenChannel;
    ssReadChannel  : ScanReadChannel;
    ssCloseChannel : ScanCloseChannel;
    ssOpenEntry    : ScanOpenEntry;
    ssReadEntry    : ScanReadEntry;
    ssCloseEntry   : ScanCloseEntry;
    ssTerminate    : ScanTerminate;
  end;
end;

procedure TForm1.AddHistoric(Msg : string; Style : TFontStyles = []; Color : TColor = clDefault);
begin
  Log.SelAttributes.Style := Style;
  Log.SelAttributes.Color := Color;
  Log.Lines.Add(Msg)
//  Memo1.Lines.Add(Msg);
end;


procedure TForm1.WebBrowserDocumentComplete(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  if (pDisp = FCurDispatch) then
  begin
    { le document est chargé, pas seulement un cadre }
    FCurDispatch := nil; {efface la variable globale }
    StatusBar.SimpleText := 'Page Internet chargée';
    // On passe a l'état suivant....
    PostMessage(Self.WindowHandle, WM_ScanState, 0, 0);
  end;
end;

procedure TForm1.WebBrowserNavigateComplete2(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  if FCurDispatch = nil then
    FCurDispatch := pDisp; { enregistre pour comparaison }
end;


procedure TForm1.Execute;
begin
  if FScanState <> ssIdle then exit;
  SetScanState(ssInit, 'Initialisation');
end;

procedure TForm1.ScanInit;
begin
  Log.Lines.Clear;
  FTTL := 24*60; // 24 heures par défaut
  AddHistoric('Scan '+DateTimeToStr(Now), [fsBold], clGreen);
  FCurChannel := 0;
  StatusBar.SimpleText := 'Lecture de la table des channels';
  AddHistoric('Lecture de '+ScriptURL, [fsBold]);
  FeedSettings.LoadFromString(HTTP.Get(ScriptURL+'?cmd=settings'));
//  FeedSettings.LoadFromString(HTTP.Get('http://static.userland.com/gems/backend/rssTwoExample2.xml'));
  AddHistoric('XML '+FeedSettings.Root.Name+' chargé', [fsBold]);
//  Memo1.Lines.Add(FeedSettings.XMLData);
  SetScanState(ssOpenChannel, 'Ouverture du channel');
end;

procedure TForm1.ScanTerminate;
begin
 Timer.Interval := FTTL*60*1000; // ms
 Timer.Enabled := true;
 AddHistoric('Terminé !', [fsBold]);
 AddHistoric('Re-démarage : '+DateTimeToStr(IncMilliSecond(Now, Timer.Interval)));
 StatusBar.SimpleText := '';
 SetScanState(ssIdle, 'Terminé !'); // WM_ScanState après lecture de la page
// SetScanState(ssIdle, 'Terminé !', False); // WM_ScanState après lecture de la page
// WebBrowser.Navigate(ScriptURL);
end;

procedure TForm1.ScanOpenChannel;
var
  URL   : string;
  Title : string;
begin
  if FCurChannel=FeedSettings.Root.Items.Count then begin
    SetScanState(ssTerminate, 'Termine la session');
    exit;
  end;
  Fils.Items.Clear;
  Title := FeedSettings.Root.Items[FCurChannel].Items.Value('Title');
  URL := FeedSettings.Root.Items[FCurChannel].Items.Value('Link');
  DateSeparator := '/';
  ShortDateFormat := 'd/m/yyyy';
  FLastBuild := StrToDateTimeDef(FeedSettings.Root.Items[FCurChannel].Items.Value('lastBuildDate'), Now);
  FNewBuild :=FLastBuild;
  AddHistoric('  Lecture de '+Title+' (dernière discussion '+DateTimeToStr(FLastBuild)+')');
  StatusBar.SimpleText := 'Chargement '+URL;
  SetScanState(ssReadChannel, Title, False); // WM_ScanState après lecture de la page
  WebBrowser.Navigate(URL);
end;

procedure TForm1.ScanReadChannel;
var
  ole_index : oleVariant;
  s : string;
begin
  // La page a été chargée, extraction des entrées de discussion...
  ole_index := 0;
  s := WebBrowser.oleobject.Document.Frames.Item(ole_index).
    document.documentElement.innerHTML;
//  Memo1.Lines.Add(s);
  // Lecture des fils
  FCurFeed := 0;
  HTMLParser.ClearConditions;
  HTMLParser.AddCondition('FIL', '<TR>', '</TD>', 0);
  HTMLParser.OnKeyFound := FeedRootFound;
  HTMLParser.AnalyseString(s);
  SetScanState(ssOpenEntry, 'Ouvre le fil');
end;

procedure TForm1.ScanCloseChannel;
var
  S : TStringList;
  SqlResult : string;
begin
  if CompareDate(FNewBuild, FLastBuild) = GreaterThanValue then begin
    FeedSettings.Root.Items[FCurChannel].Items.ItemNamed['lastBuildDate'].Value := DateTimeToStr(FNewBuild);
    S := TStringList.Create;
    S.Add('lastBuildDate='+FormatDateTime('yyyy-mm-dd hh:nn:ss', FNewBuild));
    AddHistoric('  Mise à jour du flux RSS '+FeedSettings.Root.Items[FCurChannel].Items.Value('id'), [fsBold]);
    SqlResult := HTTP.Post(ScriptURL+'?cmd=upd&rss='+FeedSettings.Root.Items[FCurChannel].Items.Value('id'), S);
    if SqlResult<>'OK' then AddHistoric(' Erreur SQL : '+SqlResult, [], clRed);
    S.Free;
  end;
  FTTL := min(FTTL, FeedSettings.Root.Items[FCurChannel].Items.ItemNamed['ttl'].IntValue);
  Inc(FCurChannel);
  SetScanState(ssOpenChannel, 'Ouverture du channel');
end;

procedure TForm1.ScanOpenEntry;
var
  Feed : TFeedEntry;
  URL  : string;
begin
  if FCurFeed=Fils.Items.Count then begin
    SetScanState(ssCloseChannel, 'Fermeture du channel');
    exit;
  end;
  if not Fils.Checked[FCurFeed] then begin
    SetScanState(ssCloseEntry, 'Fermeture du fil');
    exit;
  end;


  Feed := TFeedEntry(Fils.Items.Objects[FCurFeed]);
  URL:='http://cvfc.univ-fcomte.fr/cv/www/bureau/litfil.asp?ligne='+Feed.guid;

  StatusBar.SimpleText := 'Fil '+Feed.title+'  '+DateToStr(Feed.pubDate);
  AddHistoric('    Analyse du '+StatusBar.SimpleText);
//  AddHistoric('    Analyse "'+Fils.Items[FCurFeed]+'"');
  SetScanState(ssReadEntry, 'Lit fil'+Feed.title+'  '+DateToStr(Feed.pubDate), False);
  WebBrowser.Navigate(URL);
end;

procedure TForm1.ScanReadEntry;
var
  s : string;
begin
  s := WebBrowser.oleobject.Document.documentElement.innerHTML;
//  Memo1.Lines.Add(s);
    // Lecture des discussions
  HTMLParser.ClearConditions;
  HTMLParser.AddCondition('ITEM', '<DIV>', '</DIV>', 0);
  HTMLParser.OnKeyFound := FeedEntryFound;
  FCounterDIV := 0; // Compte les <DIV>...</DIV>
  HTMLParser.AnalyseString(s);
//  Memo1.Lines.Add(FeedSettings.XMLData);
  SetScanState(ssCloseEntry, 'Ferme le fil');
end;

procedure TForm1.ScanCloseEntry;
begin
  Inc(FCurFeed);
  SetScanState(ssOpenEntry, 'Ouvre le fil');
end;


procedure TForm1.FeedRootFound(Sender: TObject; Key, Results,
  OriginalLine: String);
var
  Entry : TFeedEntry;

  function NextToken(b, e : string) : string;
  var
    p : integer;
  begin
    p := Pos(b, Results);
    Delete(Results, 1, p+Length(b)-1);
    if e<>'' then p := Pos(e, Results) else p := 256;
    result := Copy(Results, 1, p-1);
  end;

begin
  if Pos('<TD>&nbsp;Auteur', Results) <>0 then exit; // Ligne de titre
  Entry := TFeedEntry.Create;
    Entry.author := NextToken(';', CRLF);
    NextToken(';', CRLF); // Nb
    DateSeparator := '/';
    ShortDateFormat := 'd/m/yyyy';
    Entry.pubDate := StrToDateDef(NextToken(';', CRLF), Now);
    Entry.guid := NextToken('ligne=', '"');
    Entry.title := NextToken('</A>&nbsp;', '');
    Entry.description := '';
  Fils.AddItem(Entry.title, Entry);
  Fils.Checked[Fils.Count-1]:= not (CompareDate(Entry.pubDate, FLastBuild) = LessThanValue);
end;

procedure TForm1.FeedEntryFound(Sender: TObject; Key, Results,
  OriginalLine: String);
var
  Feed : TFeedEntry;
  S    : TStringList;
  SqlResult : string;
  s1 : string;

  function NextToken(b, e : string) : string;
  var
    p : integer;
  begin
    p := Pos(b, Results);
    Delete(Results, 1, p+Length(b)-1);
    if e<>'' then p := Pos(e, Results) else p := 256;
    result := Trim(Copy(Results, 1, p-1));
  end;

begin
  Inc(FCounterDIV);
  if FCounterDIV=1 then begin
    // Root de la discussion
    Feed := TFeedEntry(Fils.Items.Objects[FCurFeed]);
    FCurGuid   := Feed.guid;
    FCurTitle  := NextToken('<FONT color=red>', '</FONT>');
    FCurAuthor := NextToken('</FONT> de ', '&nbsp;');
    DateSeparator := '/';
    ShortDateFormat := 'd/m/yyyy';
    FCurDate   := StrToDateTimeDef(NextToken('&nbsp; ', '<A'), Now);
    if CompareDate(FCurDate, FNewBuild) = GreaterThanValue	 then FNewBuild :=FCurDate;
    Exit;
  end;

  if (FCounterDIV mod 2) = 1 then begin
    // 1er DIV contient guid, auteur et date
    FCurGuid   := NextToken('<A href="cremesform.asp?num=', '&amp;');
    FCurAuthor := NextToken('</FONT>de ', '&nbsp;');
    DateSeparator := '/';
    ShortDateFormat := 'd/m/yyyy';
    s1 := NextToken('&nbsp;', '&nbsp; <A');
    FCurDate   := StrToDateTimeDef(s1, Now);
  end else begin
    // 2ème DIV contient la description
    if CompareDateTime(FCurDate, FLastBuild) = GreaterThanValue	 then begin
      with FeedSettings.Root.Items[FCurChannel].Items.Add('item').Items do begin
        Add('guid', FCurGuid);
        Add('title', FCurTitle);
        Add('description', Results);
        Add('author', FCurAuthor);
        Add('pubDate', DateTimeToStr(FCurDate));
      end;
      S := TStringList.Create;
      S.Add('id='+FeedSettings.Root.Items[FCurChannel].Items.Value('id'));
      S.Add('guid='+FCurGuid);
      S.Add('title='+XMLEncode(FCurTitle));
      S.Add('description='+XMLEncode(Results));
      S.Add('author='+FCurAuthor+'@edu.univ-fcomte.fr ('+FCurAuthor+')');
      S.Add('pubDate='+FormatDateTime('yyyy-mm-dd hh:nn:ss', FCurDate));
      SqlResult := HTTP.Post(ScriptURL+'?cmd=add', S);
      if (SqlResult<>'OK') then
        if (Pos('Duplicate entry', SqlResult)=0) or (Pos('for key 1', SqlResult)=0) then
          AddHistoric('      Erreur SQL : '+SqlResult, [], clRed)
        else
          StatusBar.SimpleText := 'SQL Duplicate entry for key 1 : '+FCurGuid
      else
        AddHistoric('      Nouvelle entrée '+FCurGuid+' de '+FCurAuthor+ ' '+FormatDateTime('dd-mm-yyyy hh:nn:ss', FCurDate));
      S.Free;
    end;
  end;

//if FCounterDIV=3 then Memo1.Lines.Add(Results);

end;


procedure TForm1.Executer1Click(Sender: TObject);
begin
  Execute;
end;

procedure TForm1.Quiter1Click(Sender: TObject);
begin
  Close;
end;

initialization

DateSeparator := '/';
ShortDateFormat := 'd/m/yyyy';


end.
