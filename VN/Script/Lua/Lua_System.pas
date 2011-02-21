//******************************************************************************
//  File        : Lua_Object.pas   (rev. 1.1)
//
//  Description : Access from Lua scripts to TObject Classes
//
//******************************************************************************
//  Exported functions :
//
//   CreateObject(string className, bool CanFree [, param1, ..paramN])   return a new TObject.
//   GetObject(string Name)                          return an existing TObject.
//
//    TObject.PropertyName                     return Property Value as Variant.
//    TObject.PropertyName = (variant Value)                 set Property Value.
//    TObject:<method name>([param1, .., paramN]) call <method name>, return Result as Variant.
//    TObject:Free()                    free the object, return True on success.

// TO-DO :
//           funzioni lua da esportare :
//                 enumval(Type :???, value :string) enumstr(Type :???, value :Integer???)
//           nei metodi dove c'è var, come lo torno in Lua?
//       *   gestione del garbage collector (metafunction _gc)
//          COMMENTARE IL CODICE MENTRO ME LO RICORDO
unit Lua_System;

interface

uses Lua, Script_System;

procedure RegisterFunctions(L: Plua_State);

implementation

uses ObjAuto, ScriptableObject, TypInfo, SysUtils, LuaUtils, lauxlib, Variants, VarUtils;

const
     OBJHANDLE_STR       ='Lua_TObjectHandle';
     ARRAYPROP_STR       ='Lua_TObjectArrayProp';
     ARRAYPROPNAME_STR   ='Lua_TObjectArrayPropName';
     SETPROP_VALUE       ='Lua_TObjectSetPropvalue';
     SETPROP_INFO       ='Lua_TObjectSetPropINFO';

type
    //Record stored in lua stack to mantain TScriptObject
    TScriptObjectData = packed record
       ID  : String[20];
       Obj :TScriptObject;
    end;
    PScriptObjectData =^TScriptObjectData;

    TLuaArrayPropData = record
       ID  : String[20];
       Obj :TScriptObject;
       PropName :string;
    end;

    TLuaSetPropData = record
       ID       :String[20];
       Info     :PTypeInfo;
       Value    :string;
    end;
    PLuaSetPropData = ^TLuaSetPropData;

//==============================================================================
// "Push a Value in the Stack" Functions

function GetPropertyArrayObject(var Data : TLuaArrayPropData; L: Plua_State; Index: Integer): boolean; forward;
function LuaPush_TScriptObject(L :Plua_State; theParams :array of Variant;
                            Obj :TObject=nil; ObjClassName :String='';
                            CanFree :Boolean=True) :boolean; forward;
function LuaPushPropertyArrayObject(L: Plua_State; Obj :TScriptObject; PropName :string): boolean; forward;
function LuaPushPropertySet(L: Plua_State; TypeInfo :PTypeInfo; PropValue :Variant): boolean; forward;


function PushProperty(L :Plua_State; scripter :TScriptObject;
                      PropName :string; PropValue :Variant; PropType :PTypeInfo) :Integer;
begin
     Result :=0;

     if (PropType^.Kind = tkClass)
     then begin
               //If this property is a class, push a TScriptObject in the stack
               if LuaPush_TScriptObject(L, [], TObject(Integer(PropValue)))
               then Result := 1;
          end
     else
     if (PropType^.Kind =tkArray)
     then begin //If this property is an array, push an ArrayPropObject in the stack
               if LuaPushPropertyArrayObject(L, scripter, PropName)
               then Result := 1;
          end
     else
     if (PropType^.Kind =tkSet)
     then begin //If this property is an array, push an ArrayPropObject in the stack
               if LuaPushPropertySet(L, PropType, PropValue)
               then Result := 1;
          end
     else
     begin //Push the PropValue
          LuaPushVariant(L, PropValue);
          Result := 1;
     end;
end;

function PushMethodResult(L :Plua_State; scripter :TScriptObject; Value :Variant; ValueType :PReturnInfo) :Integer;
begin
     Result :=0;

     if (ValueType^.ReturnType^.Kind = tkClass)
     then begin
               //If this property is a class, push a TScriptObject in the stack
               if LuaPush_TScriptObject(L, [], TObject(Integer(Value)))
               then Result := 1;
          end
     else
     if (ValueType^.ReturnType^.Kind =tkSet)
     then begin //If this property is an array, push an ArrayPropObject in the stack
               if LuaPushPropertySet(L, ValueType^.ReturnType^, Value)
               then Result := 1;
          end
     else
     begin //Push the PropValue
          LuaPushVariant(L, Value);
          Result := 1;
     end;
end;


//==============================================================================
// Array Properties Support

function Lua_IsPropertyArrayObject(L: Plua_State; Index: Integer): boolean;
begin
     Result :=False;
     try
        Result :=(LuaGetTableString(L, Index, 'ID')=ARRAYPROP_STR);
     except
     end;
end;

function GetPropertyArrayObject(var Data : TLuaArrayPropData; L: Plua_State; Index: Integer): boolean;
begin
     Result :=false;
     try
        Data.Obj :=TScriptObject(LuaGetTableLightUserData(L, Index, ARRAYPROP_STR));
        Data.PropName :=LuaGetTableString(L, Index, ARRAYPROPNAME_STR);
        Result :=true;
     except
     end;
end;

function Lua_ArrayProp_Get(L: Plua_State): Integer; cdecl;
Var
   indice,
   PropValue    :Variant;
   PropType     :PTypeInfo;
   GetPropOK    :Boolean;
   NParams      :Integer;
   Data         :TLuaArrayPropData;

begin
     Result := 0;
     NParams := lua_gettop(L);

     if (NParams=2) then
     try
        GetPropertyArrayObject(Data, L, 1);
        indice :=LuaToVariant(L, 2);

        if (indice<>NULL)
        then begin
                  PropType :=Data.Obj.GetArrayPropType(Data.PropName, indice);

                  GetPropOK := (PropType<>nil);

                  if GetPropOK
                  then PropValue :=Data.Obj.GetArrayProp(Data.PropName, indice)
                  else PropValue :=NULL;

                  Result :=PushProperty(L, Data.Obj, Data.PropName, PropValue, PropType);
             end
        else raise Exception.CreateFmt('Trying to index %s.%s with a NULL index', [Data.Obj.InstanceObj.ClassName, Data.PropName]);

     except
        On E:Exception do begin
                               LuaError(L, ERR_Script+E.Message);
                          end;

     end;
end;

function Lua_ArrayProp_Set(L: Plua_State): Integer; cdecl;
begin
end;

function LuaPushPropertyArrayObject(L: Plua_State; Obj :TScriptObject; PropName :string): boolean;
begin
     lua_newtable(L);
     LuaSetTableString(L, -1, 'ID', ARRAYPROP_STR);
     LuaSetTableLightUserData(L, -1, ARRAYPROP_STR, Obj);
     LuaSetTableString(L, -1, ARRAYPROPNAME_STR, PropName);
     LuaSetTablePropertyFuncs(L, -1, Lua_ArrayProp_Get, Lua_ArrayProp_Set);
     Result :=true;
end;

//==============================================================================
//Set Properties Support

function Lua_IsPropertySet(L: Plua_State; Index: Integer): boolean;
begin
     Result :=False;
     try
        Result :=lua_istable(L, Index) and (LuaGetTableString(L, Index, 'ID')=SETPROP_VALUE);
     except
     end;
end;

function GetPropertySet(Data :PLuaSetPropData; L: Plua_State; Index: Integer): String;
begin
     Result :='';
     if Data<>nil then FillChar(Data^, Sizeof(TLuaSetPropData), 0);

     try
        if lua_istable(L, Index)
        then begin
                  Result :=LuaGetTableString(L, Index, SETPROP_VALUE);
                  if Data<>nil then
                  begin
                       Data^.ID :=LuaGetTableString(L, Index, 'ID');
                       Data^.Info :=LuaGetTableLightUserData(L, Index, SETPROP_INFO);
                  end;
             end
        else begin
                  if (lua_isString(L, Index)=1)
                  then Result :=LuaToString(L, Index);
             end;

        if Data<>nil
        then Data^.Value :=Result;
     except
     end;
end;

function Lua_SetProp_Add(L: Plua_State): Integer; cdecl;
Var
   Val1, Val2,
   EnumName,
   xResult      :String;
   NParams      :Integer;
   pVal2        :PChar;

begin
     Result := 0;
     NParams := lua_gettop(L);

     if (NParams=2) then
     try
        //TO-DO : Controllare se TypeInfo in PLuaSetPropData è nil, in questo caso
        //        il valore è un intero e non una stringa
        //        (xchè è stato tornato da un metodo e non da una property)
        Val1 :=GetPropertySet(nil, L, 1);
        if (Val1='')
        then raise Exception.Create('Left Side is Not a Set');

        Val2 :=GetPropertySet(nil, L, 2);
        if (Val2='')
        then raise Exception.Create('Right Side is Not a Set');

        //Operation is : xResult := Val1 + Val2
        xResult :=Val1;
        pVal2 :=PChar(Val2);
        EnumName := SetNextWord(pVal2);
        while (EnumName<>'') do
        begin
             //If this Value (Val2) is not already present in Result, add it
             if (Pos(EnumName, xResult)<1)
             then xResult :=xResult+','+EnumName;
             EnumName := SetNextWord(pVal2);
        end;
        LuaPushPropertySet(L, nil, xResult);
        Result :=1;
     except
        On E:Exception do begin
                               LuaError(L, ERR_Script+E.Message);
                          end;

     end;
end;

function Lua_SetProp_Sub(L: Plua_State): Integer; cdecl;
Var
   Val1, Val2,
   EnumName,
   xResult      :String;
   NParams      :Integer;
   pVal2        :PChar;
   xPos         :Integer;

begin
     Result := 0;
     NParams := lua_gettop(L);

     if (NParams=2) then
     try
        //TO-DO : Controllare se TypeInfo in PLuaSetPropData è nil, in questo caso
        //        il valore è un intero e non una stringa
        //        (xchè è stato tornato da un metodo e non da una property)
        Val1 :=GetPropertySet(nil, L, 1);
        if (Val1='')
        then raise Exception.Create('Left Side is Not a Set');

        Val2 :=GetPropertySet(nil, L, 2);
        if (Val2='')
        then raise Exception.Create('Right Side is Not a Set');

        //Operation is : xResult := Val1 - Val2
        xResult :=Val1;
        pVal2 :=PChar(Val2);
        EnumName := SetNextWord(pVal2);
        while (EnumName<>'') do
        begin
             xPos := Pos(EnumName, xResult);
             //Delete all the occurence of this Value (Val2) in Result
             while (xPos>0) do
             begin
                  Delete(xResult, xPos, Length(EnumName)+1);
                  xPos := Pos(EnumName, xResult);
             end;
             EnumName := SetNextWord(pVal2);
        end;
        LuaPushPropertySet(L, nil, xResult);
        Result :=1;
     except
        On E:Exception do begin
                               LuaError(L, ERR_Script+E.Message);
                          end;

     end;
end;

function LuaPushPropertySet(L: Plua_State; TypeInfo :PTypeInfo; PropValue :Variant): boolean;
begin
     lua_newtable(L);
     LuaSetTableString(L, -1, 'ID', SETPROP_INFO);
     LuaSetTableLightUserData(L, -1, SETPROP_INFO, TypeInfo);

     if (TVarData(PropValue).VType = varString) or
        (TVarData(PropValue).VType = varOleStr) or
        (TypeInfo = nil)
     then LuaSetTableString(L, -1, SETPROP_VALUE, PropValue)
     else LuaSetTableString(L, -1, SETPROP_VALUE, Script_System.SetToString(TypeInfo, PropValue, false));

     LuaSetMetaFunction(L, -1, '__add', Lua_SetProp_Add);
     LuaSetMetaFunction(L, -1, '__sub', Lua_SetProp_Sub);
     Result :=true;
end;

//==============================================================================

function GetTScriptObject(L: Plua_State; Index: Integer): TScriptObject;
Var
   pObjData :PScriptObjectData;

begin
     //Result := TScriptObject(LuaGetTableLightUserData(L, Index, OBJHANDLE_STR));

     Result :=nil;
     try
        if (lua_isuserdata(L, Index)=1) then
        begin
             pObjData :=lua_touserdata(L, Index);
             if (pObjData^.ID=OBJHANDLE_STR)
             then Result :=pObjData^.Obj;
        end;
     except
     end;
end;


//=== Methods Access functions =================================================

procedure VariantToByRef(Source :Variant; var Dest :Variant);
begin
     VarClear(Dest);
     TVarData(Dest).VType :=TVarData(Source).VType or varByRef;
     if ((TVarData(Source).VType and varByRef)<>0)
     then TVarData(Dest).VPointer := TVarData(Source).VPointer
     else begin
               case (TVarData(Source).VType and varTypeMask) of // strip off modifier flags
               varEmpty, varNull: TVarData(Dest).VPointer :=nil;
               varSmallint:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Smallint));
                                PSmallInt(TVarData(Dest).VPointer)^ :=TVarData(Source).VSmallInt;
                           end;
               varInteger:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Integer));
                                PInteger(TVarData(Dest).VPointer)^ :=TVarData(Source).VInteger;
                           end;
               varSingle:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Single));
                                PSingle(TVarData(Dest).VPointer)^ :=TVarData(Source).VSingle;
                           end;
               varDouble:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Double));
                                PDouble(TVarData(Dest).VPointer)^ :=TVarData(Source).VDouble;
                           end;
               varCurrency:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Currency));
                                PCurrency(TVarData(Dest).VPointer)^ :=TVarData(Source).VCurrency;
                           end;
               varDate:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Date));
                                PDate(TVarData(Dest).VPointer)^ :=TVarData(Source).VDate;
                           end;
               varOleStr:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(PWideChar));
                                PPWideChar(TVarData(Dest).VPointer)^ :=TVarData(Source).VOleStr;
                           end;
               varDispatch:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(IDispatch));
                                PDispatch(TVarData(Dest).VPointer)^ :=IDispatch(TVarData(Source).VDispatch);
                           end;
               varError:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Cardinal));
                                PDate(TVarData(Dest).VPointer)^ :=TVarData(Source).VError;
                           end;
               varBoolean:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(WordBool));
                                PWordBool(TVarData(Dest).VPointer)^ :=TVarData(Source).VBoolean;
                           end;
               varVariant:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Variant));
                                PVariant(TVarData(Dest).VPointer)^ :=Source;
                           end;
               varUnknown:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(IInterface));
                                PUnknown(TVarData(Dest).VPointer)^ :=IInterface(TVarData(Source).VUnknown);
                           end;
               varShortInt:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(ShortInt));
                                PShortInt(TVarData(Dest).VPointer)^ :=TVarData(Source).VShortInt;
                           end;
               varByte:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Byte));
                                PByte(TVarData(Dest).VPointer)^ :=TVarData(Source).VByte;
                           end;
               varWord:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(Word));
                                PWord(TVarData(Dest).VPointer)^ :=TVarData(Source).VWord;
                           end;
               varLongWord:begin
                                GetMem(TVarData(Dest).VPointer, sizeof(LongWord));
                                PLongWord(TVarData(Dest).VPointer)^ :=TVarData(Source).VLongWord;
                           end;
               end;
          end;
end;

function ParamSameType(VarParam :Variant; ParamISOBJ, ParamISSET :Boolean;
                       ParamType :PPTypeInfo):Boolean;
begin
     if (ParamType<>nil)
     then
     Case ParamType^.Kind of
     tkUnknown : Result := (TVarData(VarParam).VType and varTypeMask) in [varEmpty, varNull, varUnknown];
     tkInteger : Result := (TVarData(VarParam).VType and varTypeMask) in [varByte, varShortInt, varWord, varInteger, varLongWord];
     tkChar    : Result := (TVarData(VarParam).VType and varTypeMask) in [varByte, varShortInt];
     //tkEnumeration,
     tkFloat   : Result := (TVarData(VarParam).VType and varTypeMask) in [varByte, varShortInt, varWord, varInteger, varLongWord,
                                                                          varSingle, varDouble];
     tkWChar,
     tkLString,
     tkWString,
     tkString  : Result := ((TVarData(VarParam).VType and varTypeMask) in [varOleStr, varStrArg]) or
                           ((TVarData(VarParam).VType and varTypeMask)=varString) ;
     tkSet     : Result := ParamISSET;
     tkClass   : Result := ParamISOBJ or (VarParam=NULL);
     //tkMethod,
     tkVariant : Result :=True;

     tkDynArray,
     tkArray   : Result := (TVarData(VarParam).VType and varArray)<>0;
     //tkRecord,
     //tkInterface,
     tkInt64   : Result := (TVarData(VarParam).VType and varTypeMask) in [varByte, varShortInt, varWord, varInteger, varLongWord, varInt64];
     else Result :=true;
     end
     else Result :=true;
end;

function Lua_TObject_Methods(L: Plua_State): Integer; cdecl;
var
  NParams,
  iParams,
  invParams    :Integer;
  theParams    :array of Variant;
  xResult,
  xVarResult,
  xTryResult   :Variant;
  ParamISOBJ,
  ParamISSET   :Boolean;
  MethodName   :String;
  curComponent :TObject;
  NewObj,
  LuaObj       :TScriptObject;
  PropSetData  :TLuaSetPropData;
  retValue     :Variant;
  MethodInfo: PMethodInfoHeader;
  ReturnInfo: PReturnInfo;
  ParamInfos: TParamInfos;
  curParamInfo :PParamInfo;
  VarIndexes   :array of Integer;
  NVarIndexes  :Integer;
  CheckParams  :Boolean;

begin
     Result :=0;
     NParams := lua_gettop(L);
     try
        LuaObj :=GetTScriptObject(L, 1);

        MethodName :=LuaGetCurrentFuncName(L);

        //Get Infos about the Method
        SetLength(ParamInfos, 0);
        LuaObj.GetMethodInfos(MethodName, MethodInfo, ReturnInfo, ParamInfos);
        SetLength(VarIndexes, 0);
        NVarIndexes :=0;
        CheckParams :=(MethodInfo<>nil) and (High(ParamInfos)>0);

        //Fill Params for Method call in inverse sequense (why???)
        SetLength(theParams, (NParams-1));
        invParams :=0;
        for iParams :=NParams downto 2  do
        begin
             //If Param[iParams] is an Object get it's real value
             ParamISOBJ :=false;
             NewObj :=GetTScriptObject(L, iParams);
             ParamISOBJ :=(NewObj<>nil);

             if ParamISOBJ
             then xResult :=Integer(NewObj.InstanceObj)
             else begin
                       ParamISSET :=Lua_IsPropertySet(L, iParams);
                       if ParamISSET
                       then begin
                                 xResult :=GetPropertySet(@PropSetData, L, iParams);
                                 try
                                   //try to convert string value to Real Set Value
                                   if (PropSetData.Info<>nil)
                                   then xTryResult :=Script_System.StringToSet(PropSetData.Info, xResult);
                                   xResult :=xTryResult;
                                 except
                                 end;
                            end
                       else xResult :=LuaToVariant(L, iParams);
                  end;

             if CheckParams
             then begin
                       curParamInfo :=ParamInfos[NParams-invParams-2];
                       if (curParamInfo<>nil) then
                       begin
                            //Check the Param Type...
                            if not(ParamSameType(xResult, ParamISOBJ, ParamISSET, PPTypeInfo(curParamInfo^.ParamType)))
                            then raise Exception.CreateFmt('%s Param %d type mismatch', [MethodName, invParams]);
                           (* if ((xResult=NULL) or ParamISOBJ) and
                               (curParamInfo^.ParamType^.Kind=tkClass)
                            then xResult :=VarAsType(xResult, vtClass); *)

                            if (ObjAuto.pfVar in curParamInfo^.Flags) or
                               (ObjAuto.pfOut in curParamInfo^.Flags)
                            then begin //Convert Param passed by Value to a Var Value
                                      SetLength(VarIndexes, NVarIndexes+1);
                                      VarIndexes[NVarIndexes] :=invParams;
                                      VariantToByRef(xResult, theParams[invParams]);

                                      inc(NVarIndexes);
                                 end
                            else theParams[invParams] :=xResult;
                       end else theParams[invParams] :=xResult;
                  end
             else begin
                      (* if (ParamISOBJ)
                       then xResult :=VarAsType(xResult, vtClass); *)

                       theParams[invParams] :=xResult;
                  end;
             inc(invParams);
        end;

        retValue := LuaObj.CallMethod(MethodName, theParams, true);

        //Push Vars on The Stack
        for iParams :=0 to High(VarIndexes) do
        begin
             VariantCopyInd(TVarData(xResult), TVarData(theParams[iParams]));
             LuaPushVariant(L, xResult);
             Inc(Result);
             FreeMem(TVarData(theParams[iParams]).VPointer);
        end;
        SetLength(VarIndexes, 0);

        //Push Function Result on the Stack
        if (retValue<>NULL)
        then Inc(Result, PushMethodResult(L, LuaObj, retValue, ReturnInfo));

     except
        On E:Exception do begin
                               LuaError(L, ERR_Script+E.Message);
                          end;

     end;
end;

function Lua_TObject_GetProp(L: Plua_State): Integer; cdecl; forward;
function Lua_TObject_SetProp(L: Plua_State): Integer; cdecl; forward;
function Lua_TObject_Free(L: Plua_State): Integer; cdecl; forward;

function LuaPush_TScriptObject(L :Plua_State; theParams :array of Variant;
                            Obj :TObject=nil; ObjClassName :String='';
                            CanFree :Boolean=True) :boolean;
var
  NParams,
  iParams      :Integer;
  xResult      :Variant;
  retValue     :Variant;
  ClassData    :PScriptClassesListData;
  LuaObj       :TScriptObject;
  NewObj       :TObject;
  CanCreate    :Boolean;
  LuaClass     :TScriptObjectClass;
  pObjData     :PScriptObjectData;
  scripter :TScriptableObject;

begin
     Result :=false;

     CanCreate := (Obj=nil);

     if CanCreate
     then ClassData :=ScriptClassesList.Find(ObjClassName)
     else ClassData :=ScriptClassesList.Find(Obj.ClassType);

     if (ClassData=nil)
     then LuaClass :=TScriptObject
     else LuaClass :=ClassData^.LuaClass;

     pObjData :=lua_newuserdata(L, sizeof(TScriptObjectData));

     if CanCreate
     then begin
               if (LuaClass<>nil)
               then begin
                         LuaObj :=LuaClass.Create(nil, false);
                         theParams[High(theParams)] :=Integer(ClassData^.ObjClass);

                         try
                            retValue :=LuaObj.CallMethod('ScriptCreate', theParams, true);
                            if (retValue<>NULL)
                            then NewObj :=TObject(Integer(retValue))
                            else NewObj :=ClassData^.ObjClass.Create;
                         except
                            NewObj :=ClassData^.ObjClass.Create;
                         end;
                         LuaObj.Free;
                         LuaObj :=LuaClass.Create(NewObj, CanFree);
                    end
               else begin
                         NewObj :=ClassData^.ObjClass.Create;
                         LuaObj :=LuaClass.Create(NewObj, CanFree);
                    end;
          end
     else begin
               if (LuaClass<>nil)
               then LuaObj :=LuaClass.Create(Obj, false)
               else LuaObj :=TScriptObject.Create(Obj, false);
          end;

     pObjData^.ID :=OBJHANDLE_STR;
     pObjData^.Obj :=LuaObj;
     LuaSetTablePropertyFuncs(L, -1, Lua_TObject_GetProp, Lua_TObject_SetProp);
     LuaSetMetaFunction(L, -1, '__gc', Lua_TObject_Free);

     Result :=true;
end;

//=== Properties Access methods ================================================

(*
//NON FUNZIONA!!!!!!! PERCHE' FA LA LISTA SOLO DELLE PROPERTY PUBLISHED
function FindPredefArrayProp(AComponent :TObject):String;
Var
   Props       :PPropList;
   i, n        :Integer;
   curPropInfo :PPropInfo;

begin
     Result :='';
     n :=GetPropList(AComponent, Props);
     for i:=0 to n-1 do
     begin
          curPropInfo :=PPropInfo(Props^[i]);
          if ((curPropInfo^.Default and $80000000)<>0) and
             (curPropInfo^.PropType^^.Kind = tkArray)
          then begin
                    Result :=curPropInfo^.Name;
               end;
     end;
end;
*)
function Lua_TObject_GetProp(L: Plua_State): Integer; cdecl;
Var
   ty           :Integer;
   PropName     :String;
   PropValue    :Variant;
   PropInfo     :PPropInfo;
   PropType     :PTypeInfo;
   GetPropOK    :Boolean;
   NParams      :Integer;
   ClassData    :PScriptClassesListData;
   LuaObj       :TScriptObject;
   NewObj       :TObject;

  function TryGetProp(AComponent :TObject; PropName :String; PropKind :TTypeKinds):Boolean;
  Var
     AsString :Boolean;

  begin
       Result :=false;
       try
          PropInfo :=GetPropInfo(AComponent, PropName, PropKind);
          if (PropInfo<>nil)
          then begin //This Name is a Property
                    PropType :=PropInfo^.PropType^;

                    //Return as String only if the property is a Set or an Enum, exclude the Boolean
                    if (PropType^.Kind=tkEnumeration)
                    then AsString := not(GetTypeData(PropType)^.BaseType^ = TypeInfo(Boolean))
                    else AsString := (PropType^.Kind=tkSet);

                    PropValue :=GetPropValue(AComponent, PropInfo, AsString);

                    Result :=true;
               end;
       except
       end;
  end;

  function TryGetPublicProp:Boolean;
  Var
     AsString :Boolean;

  begin
       Result :=false;
       try
          PropInfo :=LuaObj.GetPublicPropInfo(PropName);
          if (PropInfo<>nil)
          then begin //This Name is a Property
                    PropType :=PropInfo^.PropType^;

                    //Return as String only if the property is a Set or an Enum, exclude the Boolean
                    if (PropType^.Kind=tkEnumeration)
                    then AsString := not(GetTypeData(PropType)^.BaseType^ = TypeInfo(Boolean))
                    else AsString := (PropType^.Kind=tkSet);

                    if (PropType^.Kind<>tkArray)
                    then PropValue :=GetPropValue(LuaObj.InstanceObj, PropInfo, AsString);

                    Result :=true;
               end;
       except
       end;
  end;

  procedure GetPredefArrayProp(Index: Integer);
  var
    indice :Variant;

  begin
       GetPropOK :=false;
       try
          indice :=LuaToVariant(L, Index);
          PropName :=LuaObj.GetDefaultArrayProp;
          PropType :=LuaObj.GetArrayPropType(PropName, indice);

          GetPropOK := (PropType<>nil);
       except
       end;

       if GetPropOK
       then PropValue :=LuaObj.GetArrayProp(PropName, indice)
       else PropValue :=NULL;
  end;


begin
     Result := 0;
     GetPropOK := false;
     NParams := lua_gettop(L);

     if (NParams>0) then
     try
        LuaObj :=GetTScriptObject(L, 1);

        ty := lua_type(L, 2);
        if (ty = LUA_TNUMBER)
        then GetPredefArrayProp(2)
        else begin
                  PropName :=LuaToString(L, 2);

                  GetPropOK :=TryGetProp(LuaObj, PropName, tkProperties);
                  if not(GetPropOK)
                  then begin  //Is not a Property published in the TScriptObject, try the TObject
                            GetPropOK :=TryGetProp(LuaObj.InstanceObj, PropName, tkProperties);

                            if not(GetPropOK)
                            then begin  //Try with public Properties using scripter.GetPublicXXX
                                      GetPropOK :=TryGetPublicProp;
                                 end;


                            if not(GetPropOK)
                            then begin  //Try with public elements using scripter.GetElementXXX
                                      try
                                         PropType :=LuaObj.GetElementType(PropName);
                                         GetPropOK := (PropType<>nil);
                                      except
                                      end;

                                      if GetPropOK and (PropType^.Kind<>tkArray)
                                      then PropValue :=LuaObj.GetElement(PropName);
                                 end;
                       end;
             end;

        if (GetPropOK)
        then begin //This Name is a Property
                  Result :=PushProperty(L, LuaObj, PropName, PropValue, PropType);
             end
        else begin //This Name may be a Method or an Event ???????????
                   //TO-DO : Testare se è un evento (OnXXXX), in questo caso
                   //        tornare l' eventuale nome della funzione lua

                   // (this code is for method)
                   //LuaRawSetTableNil(L, 1, PropName);
                   //LuaRawSetTableFunction(L, 1, PropName, Lua_TObject_Methods);

                   lua_pushcfunction(L, Lua_TObject_Methods);
                   Result := 1;
             end;
     except
        On E:Exception do begin
                               LuaError(L, ERR_Script+E.Message);
                          end;

     end;
end;


//      TObject:SetProp(string PropName, variant Value)      set Property Value.
function Lua_TObject_SetProp(L: Plua_State): Integer; cdecl;
Var
   curComponent :TObject;
   ty           :Integer;
   PropName     :String;
   PropInfo     :PPropInfo;
   PropType     :PTypeInfo;
   SetPropOK    :Boolean;
   NewVal       :Variant;
   NParams      :Integer;
   ClassData    :PScriptClassesListData;
   LuaObj       :TScriptObject;
   NewObj       :TScriptObject;
   NewValISPropertySet,
   NewValISOBJ  :Boolean;
   PropertySetData :TLuaSetPropData;

  function TrySetProp(AComponent :TObject; PropName :String; PropKind :TTypeKinds):Boolean;
  begin
       Result :=false;
       try
          PropInfo :=GetPropInfo(AComponent, PropName, PropKind);
          if (PropInfo<>nil)
          then begin //This Name is a Property
                    curComponent :=AComponent;
                    PropType :=PropInfo^.PropType^;
                    Result :=true;
               end;
       except
       end;
  end;

  function TrySetPublicProp:Boolean;
  begin
       Result :=false;
       try
          PropInfo :=LuaObj.GetPublicPropInfo(PropName);
          if (PropInfo<>nil)
          then begin //This Name is a Property
                    curComponent :=LuaObj.InstanceObj;
                    PropType :=PropInfo^.PropType^;
                    Result :=true;
               end;
       except
       end;
  end;

  procedure SetPredefArray(Index: Integer);
  var
    indice :Variant;

  begin
       indice :=LuaToVariant(L, Index);
       PropName :=LuaObj.GetDefaultArrayProp;
       PropType :=LuaObj.GetArrayPropType(PropName, indice);

       if (PropType^.Kind=tkClass)
       then begin
                 if NewValISOBJ
                 then LuaObj.SetArrayProp(PropName, indice, Integer(NewObj.InstanceObj))
                 else raise Exception.Createfmt('Cannot assign %s to %s.%s', [NewVal, curComponent.ClassName, PropName]);
            end
       else LuaObj.SetArrayProp(PropName, indice, NewVal);
  end;


begin
     Result := 0;

     ty :=lua_type(L, 3);
     if (ty <> LUA_TFUNCTION) then
     try
        LuaObj :=GetTScriptObject(L, 1);

        //If the new Value is an Object get it's real value
        NewValISOBJ :=false;
        NewObj :=GetTScriptObject(L, 3);
        NewValISOBJ :=(NewObj<>nil);

        if not(NewValISOBJ)
        then begin
                  NewValISPropertySet :=Lua_IsPropertySet(L, 3);
                  if NewValISPropertySet
                  then NewVal :=GetPropertySet(@PropertySetData, L, 3)
                  else NewVal :=LuaToVariant(L, 3);
             end;

        ty := lua_type(L, 2);
        if (ty = LUA_TNUMBER)
        then SetPredefArray(2)
        else begin
                  PropName :=LuaToString(L, 2);

                  SetPropOK :=TrySetProp(LuaObj, PropName, tkProperties);
                  if not(SetPropOK)
                  then begin  //Is not a Property published in the TScriptObject, try the TObject
                            SetPropOK :=TrySetProp(LuaObj.InstanceObj, PropName, tkProperties);

                            if not(SetPropOK)
                            then begin //Try with public Properties using scripter.GetPublicPropInfo
                                      SetPropOK :=TrySetPublicProp;
                                 end;

                            if not(SetPropOK)
                            then begin  //Try with public elements using scripter.SetElementXXX
                                      try
                                         PropType :=LuaObj.GetElementType(PropName);
                                         SetPropOK := (PropType<>nil);
                                      except
                                      end;

                                      if SetPropOK then
                                      begin
                                           if (PropType^.Kind=tkClass)
                                           then begin
                                                     if NewValISOBJ
                                                     then LuaObj.SetElement(PropName, Integer(NewObj.InstanceObj))
                                                     else raise Exception.Createfmt('Cannot assign %s to %s.%s', [NewVal, LuaObj.InstanceObj.ClassName, PropName]);
                                                end
                                           else begin
                                                     if NewValISPropertySet  //convert string to real Value
                                                     then NewVal :=Script_System.StringToSet(PropertySetData.Info, NewVal);
                                                     LuaObj.SetElement(PropName, NewVal);
                                                end;
                                           Exit;
                                      end;
                                 end;
                       end;

                  if SetPropOK
                  then begin
                            if (PropType^.Kind=tkClass)
                            then begin
                                      if NewValISOBJ
                                      then MySetPropValue(curComponent, PropInfo, Integer(NewObj.InstanceObj))
                                      else raise Exception.Createfmt('Cannot assign %s to %s.%s', [NewVal, curComponent.ClassName, PropName]);
                                 end
                            else MySetPropValue(curComponent, PropInfo, NewVal);
                       end
                  else begin //This Name may be a Method or an Event ???????????
                   //TO-DO : se è un evento potremmo mantenere una Lista
                   //        che associa un oggetto con il nome di una
                   //        funzione Lua. Settiamo come evento un
                   //        nostro metodo che cerca nella Lista l' oggetto
                   //        e chiama la relativa funzione lua
                       end;
             end;
     except
        On E:Exception do begin
                               LuaError(L, ERR_Script+E.Message);
                          end;

     end;
end;


//      TObject:Free()                                          free the object.
function Lua_TObject_Free(L: Plua_State): Integer; cdecl;
Var
   theObject    :TScriptObject;
   NParams      :Integer;

begin
     Result := 0;

     NParams := lua_gettop(L);
     if (NParams=1)
     then begin
               try
                  theObject :=GetTScriptObject(L, 1);

                  //LuaEventsList.Del(theObject.InstanceObj);

                  theObject.Free;
                  //LuaSetTableClear(L, 1);

               except
                  On E:Exception do begin
                                       LuaError(L, ERR_Script+E.Message);
                                    end;
               end;
          end;
end;

//==============================================================================
// Main Functions

function Lua_CreateObject(L: Plua_State): Integer; cdecl;
var
  NParams,
  iParams,
  invParams    :Integer;
  theParams    :array of Variant;
  xResult,
  xTryResult   :Variant;
  retValue     :PVariant;
  ObjClassName :String;
  ClassData    :PScriptClassesListData;
  scripter     :TScriptObject;
  NewObj       :TScriptObject;
  CanFree,
  ParamISOBJ   :Boolean;
  PropSetData  :TLuaSetPropData;

begin
     Result :=0;
     NParams := lua_gettop(L);
     if (NParams>1)
     then begin
               try
                  ObjClassName :=LuaToString(L, 1);
                  CanFree :=LuaToBoolean(L, 2);

                  //Fill Params for Create call in inverse sequense (why???)
                  SetLength(theParams, NParams-1);
                  invParams :=0;
                  for iParams :=NParams downto 3  do
                  begin
                       //If Param[iParams] is an Object get it's real value
                       ParamISOBJ :=false;
                       NewObj :=GetTScriptObject(L, iParams);
                       ParamISOBJ :=(NewObj<>nil);

                       if ParamISOBJ
                       then xResult :=Integer(NewObj.InstanceObj)
                       else begin
                                 if Lua_IsPropertySet(L, iParams)
                                 then begin
                                           xResult :=GetPropertySet(@PropSetData, L, iParams);
                                           try
                                              //try to convert string value to Real Set Value
                                              if (PropSetData.Info<>nil)
                                              then xTryResult :=Script_System.StringToSet(PropSetData.Info, xResult);
                                              xResult :=xTryResult;
                                           except
                                           end;
                                      end
                                 else xResult :=LuaToVariant(L, iParams);
                            end;

                       theParams[invParams] :=xResult;
                       inc(invParams);
                  end;
                  //LuaPush_TScriptObject sets the last parameter with the Class
                  theParams[invParams] :=1234;

                  if LuaPush_TScriptObject(L, theParams, nil, ObjClassName, CanFree)
                  then Result :=1
                  else raise Exception.Createfmt('Cannot Create class %s', [ObjClassName])
               except
                  On E:Exception do begin
                                       LuaError(L, ERR_Script+E.Message);
                                    end;
               end;
          end;
end;


function Lua_GetObject(L: Plua_State): Integer; cdecl;
var
  NParams,
  iParams,
  invParams    :Integer;
  theParams    :array of Variant;
  xResult      :Variant;
  retValue     :PVariant;
  ObjName      :String;
  ObjData      :PScriptExistingObjListData;
  scripter     :TScriptObject;
  NewObj       :TObject;

begin
     Result :=0;
     NParams := lua_gettop(L);
     if (NParams>0)
     then begin
               try
                  ObjName :=LuaToString(L, 1);

                  ObjData := ScriptExistingObjList.Find(ObjName);
                  if (ObjData<>nil)
                  then begin
                            if LuaPush_TScriptObject(L, [], ObjData^.Obj)
                            then Result :=1
                            else raise Exception.Createfmt('Cannot Interface with class %s', [ObjData^.Obj.ClassName]);
                       end
                  else raise Exception.Createfmt('Object "%s" not found', [ObjName]);
               except
                  On E:Exception do begin
                                       LuaError(L, ERR_Script+E.Message);
                                    end;
               end;
          end;
end;


procedure RegisterFunctions(L: Plua_State);
begin
     LuaRegister(L, 'CreateObject', Lua_CreateObject);
     LuaRegister(L, 'GetObject', Lua_GetObject);
end;


end.
