unit LuaTestmain;


interface

{$O-}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActiveX, ExtCtrls;


type
{$METHODINFO ON}
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Memo2: TMemo;
    ButtonLua: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonLuaClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function TestAAA(p1, p2, p3, p4 :Integer) :Boolean;
  end;
{$METHODINFO OFF}

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Lua, LuaUtils, LuaLib, lauxlib, Lua_Assert, ScriptableObject, ObjAuto, typInfo,
     Script_System, Script_Classes, Lua_System, Script_DB, Script_DBTables, ObjComAuto;

const
     HandleStr ='HANDLE';

type
{$METHODINFO ON}
    TMyTest = class(TObjectDispatch)
    public
       constructor Create(AInstanceObj :TObject; AOwned :Boolean);
       function ScriptCreate(Obj :Integer):Integer;

       function TestA(Param1 :Boolean; Param2 :Pointer(*TComponent*); var ByRefParam :Integer; pippo :String) :TAnchors; stdcall;
       procedure TestB;
    end;
{$METHODINFO OFF}


constructor TMyTest.Create(AInstanceObj :TObject; AOwned :Boolean);
begin
     inherited;
end;

function TMyTest.ScriptCreate(Obj :Integer):Integer;
begin
     Form1.Caption :='AZZZZZZZZZZZZZZZZ';
end;

function TMyTest.TestA(Param1 :Boolean; Param2 :Pointer(*TComponent*); var ByRefParam :Integer; pippo :String) :TAnchors;
begin
     Form1.Caption :=pippo;
     Inc(ByRefParam);
end;

procedure TMyTest.TestB;
begin
     Form1.Caption :='TestB';
end;




function TForm1.TestAAA(p1, p2, p3, p4 :Integer) :Boolean;
begin
     SetBounds(p1, p2, p3, p4);
     Result :=True;
end;

function listvars(L :Plua_State; level: Integer) :Integer;
Var
  ar          :lua_Debug;
  i           :Integer;
  name        :PChar;

begin
     if (lua_getstack(L, level, @ar) = 0) then
     begin
          //* failure: no such level in the stack */
          exit;
     end;

     i := 1;
     name := lua_getlocal(L, @ar, i);
     while (name <> nil) do
     begin
          lua_pop(L, 1);

          Inc(i);
          name := lua_getlocal(L, @ar, i);
     end;

     lua_getinfo(L, 'f', @ar);

 (*    i := 1;
     name := lua_getpuvalue(L, -1, @ar);
     while ((name <> nil) do
     begin
          lua_pop(L, 1);

          Inc(i);
          name := lua_getpuvalue(L, -1, @ar);
     end; *)
end;

function GetTObject(L: Plua_State; Index: Integer): TObject;
begin
     Result := TObject(LuaGetTableLightUserData(L, Index, HandleStr));
end;


function LuaTestNames(L: Plua_State): Integer; cdecl;
var
  NParams     :Integer;

begin
     NParams := lua_gettop(L);
//     if (NParams=0)
//     then begin
     Form1.Memo2.Lines.Add(GetTObject(L, 1).ClassName);
     Form1.memo2.Lines.Add('CALLED '+LuaGetCurrentFuncName(L)+' ='+IntToStr(NParams));
     LuaPushInteger(L, 1000);
     Result :=1;
(*          end
     else LuaError(L, ERR_Script+
                          'LuaOpenDatabase must have NO Params');
*)
end;

function LuaGetProp(L: PLua_State): Integer; cdecl;

  procedure Strings(Index: Integer);
  var
    S: string;
    indice :Integer;

  begin
       indice :=luaL_checkInt(L, Index);
       S := Form1.Memo1.Lines[indice];
       LuaPushString(L, 'Line '+IntToStr(indice)+' ='+S);
  end;

var
  Name : string;
  ty, NParams   : Integer;

begin
  Result := 1;
  try
    NParams := lua_gettop(L);
    ty :=lua_type(L, 1);
    ty := lua_type(L, 2);
    if (ty = LUA_TNUMBER) then
    begin
      Strings(2);
      Exit;
    end;
    Name := LuaToString(L, 2);

    //Se esiste questo metodo

    LuaRawSetTableNil(L, 1, Name);
    LuaRawSetTableFunction(L, 1, Name, LuaTestNames);

    lua_pushcfunction(L, LuaTestNames);
  except
    on E: Exception do luaL_error(L, PChar(E.Message));
  end;
end;

function LuaSetProp(L: PLua_State): Integer; cdecl;

  procedure Strings(NewVal :Variant; Index: Integer);
  var
    S: string;
    indice :Integer;

  begin
       indice :=luaL_checkInt(L, Index);
       Form1.Memo1.Lines[indice] :=NewVal;
//       LuaPushString(L, 'Line '+IntToStr(indice)+' ='+S);
  end;

var
  Name: string;
  NewVal :Variant;
  ty, NParams    :Integer;

begin
  Result := 0;
  try
     NParams := lua_gettop(L);
     ty :=lua_type(L, 1);
     ty :=lua_type(L, 2);
     ty :=lua_type(L, 3);
     if (ty <> LUA_TFUNCTION)
     then begin
               NewVal :=LuaToVariant(L, 3);

               ty := lua_type(L, 2);
               if (ty = LUA_TNUMBER) then
               begin
                    Strings(NewVal, 2);
                    Exit;
               end;
               Name := LuaToString(L, 2);
               Form1.Label1.Caption :=Name;
               //Name := luaL_checkstring(L, 2);
               //LuaPushString(L, 'Get prop '+Name);
          end;
  except
    on E: Exception do luaL_error(L, PChar(E.Message));
  end;
end;

function LuaCreate(L: Plua_State): Integer; cdecl;
var
  NParams     :Integer;
  ar          :lua_Debug;
  err         :Integer;
  paramIndex  :Integer;
  ty          :Integer;

begin
     NParams := lua_gettop(L);

     paramIndex :=1;
    (* lua_pushnil(L);
     while (lua_next(L, 1) <> 0) do
     begin
          ty :=lua_type(L, -paramIndex);
          Dec(paramIndex);
          lua_pop(L, 1);
     end;
     *)
     lua_newtable(L);
     LuaSetTableLightUserData(L, -1, HandleStr, Form1.Button1);
     //LuaSetTableFunction(L, 1, 'vvv', vvvv);
     LuaSetTablePropertyFuncs(L, -1, LuaGetProp, LuaSetProp);
//     LuaSetMetaFunction(L, 1, '__index', );
//     LuaSetMetaFunction(L, 1, '__newindex', );
//     LuaSetMetaFunction(L, 1, '__call', LuaTestNames);
     Result :=1;
end;

procedure RunLuaScript(ScriptFile :String);
Var
   L :Plua_state;

begin
          try
             L := lua_open;
             luaopen_base(L);
             luaopen_string(L);

             Script_System.RegisterObject(Form1, 'Form1');
             Script_System.RegisterClass(TLabel);
             Script_System.RegisterClass(TMyTest);
             Lua_System.RegisterFunctions(L);

             //LuaRegister(L, 'NameAAA', LuaTestNames);
             //LuaRegister(L, 'NameBBBB', LuaTestNames);
             Lua_Assert.RegisterFunctions(L);

             LuaLoadBuffer(L, Form1.Memo1.Text, 'code');
             LuaPcall(L, 0, 0, 0);
          finally
             if (L<>Nil)
             then lua_close(L);
          end;
end;

(*
function SetToString(TypeInfo: PTypeInfo; Value: Longint; Brackets: Boolean): string;
var
  S: TIntegerSet;
  I: Integer;
begin
  Result := '';
  Integer(S) := Value;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

function StringToSet(EnumInfo: PTypeInfo; const Value: string): Integer;
var
  P: PChar;
  EnumName: string;
  EnumValue: Longint;

  // grab the next enum name
  function NextWord(var P: PChar): string;
  var
    i: Integer;
  begin
    i := 0;

    // scan til whitespace
    while not (P[i] in [',', ' ', #0,']']) do
      Inc(i);

    SetString(Result, P, i);

    // skip whitespace
    while P[i] in [',', ' ',']'] do
      Inc(i);

    Inc(P, i);
  end;

begin
  Result := 0;
  if Value = '' then Exit;
  P := PChar(Value);

  // skip leading bracket and whitespace
  while P^ in ['[',' '] do
    Inc(P);

  EnumName := NextWord(P);
  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
      raise EPropertyConvertError.CreateFmt('Invalid Property Element %s', [EnumName]);
    Include(TIntegerSet(Result), EnumValue);
    EnumName := NextWord(P);
  end;
end;
*)

function ObjectInvoke(Instance: TObject; MethodHeader: PMethodInfoHeader;
  const ParamIndexes: array of Integer;
  const Params: array of Variant): Variant; 
const
  MaxParams = 10;

  procedure Swap(var A, B: PParamInfo);
  var
    T: PParamInfo;
  begin
    T := A;
    A := B;
    B := T;
  end;

var
  MethodName: string;

  procedure ParameterMismatch(I: Integer);
  begin
    raise Exception.CreateFmt('sTypeMisMatch', [I, MethodName]);
  end;

var
  MethodInfo: Pointer;
  ReturnInfo: PReturnInfo;
  MethodAddr: Pointer;
  InfoEnd: Pointer;
  Count: Integer;
  I, K, P: Integer;
  Param: PParamInfo;
  Regs: array[paEAX..paECX] of Cardinal;
  RetVal: Variant;
  ParamType: TVarType;
  VarType: TVarType;
  ParamVarData: PVarData;
  PushData: Pointer;
  ParamBytes: Integer;
  Size: Integer;
  Frame: PChar;
  ResultParam: Pointer;
  ResultPointer: Pointer;
  ParamInfos: array[0..MaxParams- 1] of PParamInfo;
  ParamData: array[0..MaxParams - 1] of Pointer;
  Pointers: array[0..MaxParams - 1] of Pointer;
  Temps: array[0..MaxParams - 1] of Variant;
begin
  // MethodInfo now points to the method we found.
  MethodInfo := MethodHeader;
  MethodAddr := MethodHeader^.Addr;
  MethodName := PMethodInfoHeader(MethodInfo)^.Name;
  Inc(Integer(MethodInfo), SizeOf(TMethodInfoHeader) - SizeOf(ShortString) + 1 +
    Length(MethodName));
  ReturnInfo := MethodInfo;
  Inc(Integer(MethodInfo), SizeOf(TReturnInfo));

  InfoEnd := Pointer(Integer(MethodHeader) + MethodHeader^.Len);
  Count := 0;
  while Integer(MethodInfo) < Integer(InfoEnd) do
  begin
    if Count >= MaxParams then
      raise Exception.CreateFmt('sMethodOver', [MethodName, MaxParams]);
    ParamInfos[Count] := MethodInfo;
    Inc(Count);
    Inc(Integer(MethodInfo), SizeOf(TParamInfo) - SizeOf(ShortString) + 1 +
      Length(PParamInfo(MethodInfo)^.Name));
  end;

  if High(Params) >= Count then
    raise Exception.CreateFmt('sTooManyParams', [MethodName]);

(*
  // Fill the ParamData array, converting the type as necessary, taking
  // into account any ParamIndexes supplied
  P := 0;
  FillChar(ParamData, SizeOf(ParamData), 0);
  for I := 0 to High(Params) do
  begin
    // Figure out what parameter index this parameter refers to.
    // If it is a named parameter it will have an entry in the ParamIndexs
    // array. If not, P points to the current parameter to use for unnamed
    // parameters. K is the formal parameter number.
    // This calculation assumes Self is first and any result parameters are last
    if I <= High(ParamIndexes) then
    begin
      K := ParamIndexes[I];
      if K >= Count then
        raise Exception.CreateFmt('sInvalidDispID', [I, MethodName]);
    end
    else
      K := High(Params) - P + 1;  // Add one to account for Self
    Param := ParamInfos[K];
    ParamType := GetVariantType(Param^.ParamType^);
    ParamVarData := @Params[I];
    VarType := ParamVarData^.VType;
    if Param^.Flags * [pfOut, pfVar] <> [] then
    begin
                                              
      // For pfVar, the variant must be a byref and equal to the type.
      if (VarType <> ParamType or varByRef) and (ParamType <> varVariant) then
        ParameterMismatch(I);
    end
    else
                      
      // Convert the parameter to the right type
      case ConvertKindOf(VarType and varTypeMask, ParamType) of
        ckConvert:
          try
            Temps[I] := VarAsType(Params[I], ParamType);
            // The data bytes for sizes < 4 are dirty, that is they are not
            // guarenteed to have 0's in the high bytes. We need them to be zero'ed
            if ParamType <= CMaxArrayVarType then
              case CVarTypeToElementInfo[ParamType].Size of
                1: TVarData(Temps[I]).VLongWord := TVarData(Temps[I]).VByte;
                2: TVarData(Temps[I]).VLongWord := TVarData(Temps[I]).VWord;
              end;
            ParamVarData := @Temps[I];
          except
            ParameterMismatch(I);
          end;
        ckError: ParameterMismatch(I);
      end;

    if ParamType = varVariant then
    begin
      Pointers[K] := ParamVarData;
      ParamData[K] := @Pointers[K];
    end
    else if varByRef and VarType <> 0 then
      ParamData[K] := @ParamVarData^.VPointer 
    else
      ParamData[K] := @ParamVarData^.VInteger;

    // Update P which is the pointer to the current non-named parameter.
    // This assumes that unnamed parameter fill in the holes left by
    // named parameters.
    while (P <= High(Params)) and (ParamData[High(Params) - P + 1] <> nil) do
      Inc(P)
  end;

  // Set up the call frame        RET EBP
  ParamBytes := ReturnInfo^.ParamSize - (4 + 4);
  asm
            SUB     ESP,ParamBytes
            MOV     Frame,ESP
  end;
  Dec(Integer(Frame), 4 + 4); // Access numbers include RET and EBP

  // Push the parameters on the stack (or put them into the correct register)
  ResultParam := nil;
  for I := 0 to Count - 1 do
  begin
    Param := ParamInfos[I];
    PushData := ParamData[I];
    if PushData = nil then
      if (Param^.ParamType^.Kind = tkClass) and SameText(Param^.Name, 'SELF') then
        // Self is special. It doesn't appear in the ParamData array since it
        // is not represented in the Params array.
        PushData := @Instance
      else if pfResult in Param^.Flags then
      begin
        ResultParam := Param;
        VarClear(Result);
        TVarData(Result).VType := GetVariantType(Param^.ParamType^);
        if TVarData(Result).VType = varVariant then
          ResultPointer := @Result
        else
          ResultPointer := @TVarData(Result).VInteger;
        PushData := @ResultPointer;
      end
      else
                                          
        raise Exception.CreateFmt(sParamRequired, [I, MethodName]);
    if Param^.Access < Word(Ord(paStack)) then
      Regs[Param^.Access] := PCardinal(PushData)^
    else
    begin
      if [pfVar, pfOut, pfResult] * Param^.Flags <> [] then
        PCardinal(@Frame[Param^.Access])^ := PCardinal(PushData)^
      else
      begin
        Size := GetTypeSize(Param^.ParamType^);
        case Size of
          1, 2, 4:
            PCardinal(@Frame[Param^.Access])^ := PCardinal(PushData)^;
          8:
          begin
            PCardinal(@Frame[Param^.Access])^ := PCardinal(PushData)^;
            PCardinal(@Frame[Param^.Access + 4])^ :=
              PCardinal(Integer(PushData) + 4)^;
          end;
        else
          Move(PushData^, Frame[Param^.Access and not 3], Size);
        end;
      end;
    end;
  end;

  // Do the call
  asm
            MOV     EAX,DWORD PTR Regs[0]
            MOV     EDX,DWORD PTR Regs[4]
            MOV     ECX,DWORD PTR Regs[8]
            CALL    MethodAddr
            MOV     DWORD PTR Regs[0],EAX
            MOV     DWORD PTR Regs[4],EDX
  end;

  if ReturnInfo^.CallingConvention = ccCdecl then
  asm
            ADD     ESP,ParamBytes
  end;

                                           
  if (ResultParam = nil) and (ReturnInfo^.ReturnType <> nil) then
  begin
    // The result came back in registers. Otherwise a result pointer was used
    // and the return variant is already initialized (or it was a procedure)
    TVarData(RetVal).VType := GetVariantType(ReturnInfo^.ReturnType^);
    if ReturnInfo^.ReturnType^.Kind = tkFloat then
      GetFloatReturn(TVarData(RetVal).VDouble, GetTypeData(ReturnInfo^.ReturnType^)^.FloatType)
    else
    begin
      // For regular Boolean types, we must convert it to a boolean to
      // wipe the high order bytes; otherwise the caller may see a false
      // as true.
      if (TVarData(RetVal).VType = varBoolean) and
        (ReturnInfo^.ReturnType^ = System.TypeInfo(Boolean)) then
        TVarData(RetVal).VInteger := Integer(Boolean(Regs[paEAX]))
      else
        TVarData(RetVal).VInteger := Integer(Regs[paEAX]);
      PCardinal(Integer(@TVarData(RetVal).VInteger) + 4)^ := Regs[paEDX];
    end;
    Result := RetVal;
    TVarData(RetVal).VType := varEmpty;
  end;
  *)
end;


procedure TForm1.Button1Click(Sender: TObject);
var
 Scripter : TScriptableObject;
 ValArray : array of Variant;
 I : integer;
 V : variant;
 PV :PVariant;
 Nparams :Integer;
 Test    :TMyTest;
 mInfo   :PMethodInfoHeader;
 MethodName  :String;
 Props       :PPropList;
 n           :Integer;
 pInfo       :PPropInfo;
 tInfo       :PTypeInfo;
 retValue    :Variant;
 MethodInfo: PMethodInfoHeader;
 ReturnInfo: PReturnInfo;
 ParamInfos: TParamInfos;
 curParam    :PParamInfo;
 S           :TIntegerSet;


begin
     Test :=TMyTest.Create(Self, false);
     Scripter    :=TScriptableObject.Create(Self, false);
     MethodName :='TestAAA';

     Scripter.GetMethodInfos(MethodName, MethodInfo, ReturnInfo, ParamInfos);
     //ObjectInvoke(Test, GetMethodInfo(Test, 'TestA'), [0, 1, 2, 3], [true, Integer(Self), NParams, 'abcd']);
     for I :=0 to High(ParamInfos) do
     begin
          curParam :=ParamInfos[i];
          Memo2.Lines.Add(curParam^.Name+' : '+curParam^.ParamType^.Name+'; '+
                          IntToHex(curParam^.Access, 8));
     end;

     Scripter.Free;
     Test.Free;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
 Scripter : TScriptableObject;
 ValArray : array of Variant;
 I : integer;
 V : variant;
 PV :PVariant;
 Nparams :Integer;
 Test    :TScriptComponent;
 mInfo   :PMethodInfoHeader;
 MethodName  :String;
 Props       :PPropList;
 n           :Integer;
 pInfo       :PPropInfo;
 tInfo       :PTypeInfo;
 retValue    :Variant;
 MethodInfo: PMethodInfoHeader;
 ReturnInfo: PReturnInfo;
 ParamInfos: TParamInfos;
 curParam    :PParamInfo;
 S           :TIntegerSet;


begin
     Test :=TScriptComponent.Create(Self, false);
     MethodName :='TestAAA';

     Test.GetMethodInfos(MethodName, MethodInfo, ReturnInfo, ParamInfos);
     //ObjectInvoke(Test, GetMethodInfo(Test, 'TestA'), [0, 1, 2, 3], [true, Integer(Self), NParams, 'abcd']);
     for I :=0 to High(ParamInfos) do
     begin
          curParam :=ParamInfos[i];
          Memo2.Lines.Add(curParam^.Name+' : '+curParam^.ParamType^.Name+'; '+
                          IntToHex(curParam^.Access, 8));
     end;

     Test.Free;
end;


procedure TForm1.ButtonLuaClick(Sender: TObject);
begin
     RunLuaScript('');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     if OpenDialog1.InitialDir=''
     then OpenDialog1.InitialDir :=ExtractFilePath(ParamStr(0))+'scripts';
     
     if OpenDialog1.Execute
     then Memo1.Lines.LoadFromFile(OpenDialog1.FileName);

end;


end.
