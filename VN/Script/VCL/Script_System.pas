//******************************************************************************
//***                     SCRIPT VCL FUNCTIONS                               ***
//***                                                                        ***
//***        (c) Massimo Magnano 2006                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : Script_System.pas   (rev. 1.1)
//
//  Description : Access from scripts to all classes derived form TObject.
//                Useful function for property of kind "set"
//
//******************************************************************************
// Known Limitations :
//   Because VCL is not compiled whith the directives METHODINFO and TYPEINFO activated
//   in the bases classes there is two different behaviour
//    1) in Classes derived from TComponent all the methods (public and published) can be called
//    2) in Classes derived from TObject only published methods can be called,
//       so if you need to call public methods of your class this must be derived from TObjectDispatch.
//       The alternative is to declare a method with the same parameters and result in the
//       ScriptObject derived class (registered with "RegisterClass") that simply call the
//       method in InstanceObj.
//       This alternative must be used when VCL Objects must be accessed from script side.
//
//
// TO-DO :
//           Eventi degli oggetti (una classe per ogni evento + lista con il nome della funzione lua)
//           Gestione delle property di Tipo Record  Vedi cos'è PFieldTable
//       +   Metodo x avere le property publiche??? invece di usare SetElement
//       *   Gestione di TComponent.<componentname> come se fosse una property tkClass
//      ++   Migliorare la gestione delle property che sono degli array (se possibile???)
//       *   Migliorare la registrazione delle classi, andare sempre alla ricerca dell' ancestor
//          COMMENTARE IL CODICE MENTRO ME LO RICORDO


unit Script_System;
{$J+}
interface

uses Classes, Controls, TypInfo, SysUtils, ScriptableObject, Variants,
     ObjAuto, ObjComAuto, MGList;

type
{$METHODINFO ON}
//{$TYPEINFO ON}  use this if you want only published methods 
    TScriptObject = class(TObjectDispatch)
    protected
       rInstanceObj :TObject;
       Owned        :Boolean;

       class function GetPublicPropertyAccessClass :TClass; virtual;

    public
       constructor Create(AInstanceObj :TObject; AOwned :Boolean);
       destructor Destroy; override;

       function CallMethod(MethodName :String; const Args : array of variant;
                        NeedResult: Boolean): Variant;
       function GetMethodInfos(MethodName :String;
                       var MethodInfo: PMethodInfoHeader; var ReturnInfo: PReturnInfo;
                       var ParamInfos: TParamInfos):Boolean;



       //WARNING : if you want parameters when creating an Object you must write
       //          a function with the following form that create the object.
       //          You must change every parameter or result of type TClass, TObject
       //          with type integer (ObjComAuto do not support this types?????)
       //
       function ScriptCreate(ObjClass :Integer) :Integer; overload; virtual;

       function GetDefaultArrayProp :String; virtual;
       function GetArrayPropType(Name :String; index :Variant) :PTypeInfo; virtual;
       function GetArrayProp(Name :String; index :Variant) :Variant; virtual;
       procedure SetArrayProp(Name :String; index :Variant; Value :Variant); virtual;

       //If the ElementType is tkSet or tkEnumeration (except the boolean) you may return the Value
       // in GetElement as a String
       function GetElementType(Name :String) :PTypeInfo; virtual;
       function GetElement(Name :String) :Variant; virtual;
       procedure SetElement(Name :String; Value :Variant); virtual;

       function GetPublicPropInfo(Name :String) :PPropInfo;
       function GetPublicPropValue(Name :String; AsString :Boolean) :Variant;
       procedure SetPublicPropValue(Name :String; Value :Variant);


       property InstanceObj :TObject read rInstanceObj;
    end;
//{$TYPEINFO OFF}
{$METHODINFO OFF}

    TScriptObjectClass = class of TScriptObject;

const
     TypeInfoArray : TTypeInfo = (Kind : tkArray; Name :'');
     TypeInfoClass : TTypeInfo = (Kind : tkClass; Name :'');

type
    //This List associate an Object Class with a ScriptObject Class
    TScriptClassesListData = record
       ObjClass :TClass;
       LuaClass :TScriptObjectClass;
    end;
    PScriptClassesListData =^TScriptClassesListData;

    TScriptClassesList = class(TMGList)
    protected
        InternalData :TScriptClassesListData;

        function allocData :Pointer; override;
        procedure deallocData(pData :Pointer); override;
        function internalFind(ObjClassName :String) :PScriptClassesListData; overload;
        function internalFind(ObjClass :TClass) :PScriptClassesListData; overload;
    public
        function Add(ObjClass :TClass; LuaClass :TScriptObjectClass=nil) :PScriptClassesListData; overload;
        function FindAncestor(ObjClass :TClass) :TScriptObjectClass;
        function Find(ObjClassName :String) :PScriptClassesListData; overload;
        function Find(ObjClass :TClass) :PScriptClassesListData; overload;
    end;

    //This List associate an Existing Object Instance with a Name in the Lua script
    TScriptExistingObjListData = record
       Obj :TObject;
       Name :String;
    end;
    PScriptExistingObjListData =^TScriptExistingObjListData;

    TScriptExistingObjList = class(TMGList)
    protected
        function allocData :Pointer; override;
        procedure deallocData(pData :Pointer); override;
    public
        function Add(Obj :TObject; Name :String) :PScriptExistingObjListData; overload;
        function Find(Name :String) :PScriptExistingObjListData; overload;
    end;


Var
   ScriptClassesList :TScriptClassesList =nil;
   ScriptExistingObjList :TScriptExistingObjList =nil;

procedure RegisterClass(ObjClass :TClass; LuaClass :TScriptObjectClass=nil);
procedure RegisterObject(Obj :TObject; Name :String; LuaClass :TScriptObjectClass=nil);

procedure MySetPropValue(Instance: TObject; PropInfo: PPropInfo; const Value: Variant);

function SetToString(TypeInfo: PTypeInfo; Value: Integer; Brackets: Boolean): string;
function SetNextWord(var P: PChar): string;
function StringToSet(EnumInfo: PTypeInfo; const Value: string): Integer;

implementation


procedure MySetPropValue(Instance: TObject; PropInfo: PPropInfo;
  const Value: Variant);
begin
     //SetPropValue raise an exception when i try to set a class property...
     // even if it's value is a simple Integer (infact GetPropValue return it as Integer)

     if PropInfo^.PropType^.Kind = tkClass
     then SetOrdProp(Instance, PropInfo, Value)
     else SetPropValue(Instance, PropInfo, Value);
end;

constructor TScriptObject.Create(AInstanceObj :TObject; AOwned :Boolean);
begin
(*     if AInstanceObj=nil
     then AInstanceObj :=Self;
*)     inherited Create(AInstanceObj, AOwned);
     rInstanceObj :=AInstanceObj;
     Owned :=AOwned;
end;

destructor TScriptObject.Destroy;
begin
     if Owned then rInstanceObj.Free;
     inherited Destroy;
end;

function TScriptObject.ScriptCreate(ObjClass :Integer) :Integer;
begin
     Result :=Integer(TClass(ObjClass).Create);
end;

//stessa cosa x CallMethodLexicalOrder
function TScriptObject.CallMethod(MethodName :String; const Args : array of variant;
                               NeedResult: Boolean): Variant;
Var
   scripter :TScriptableObject;
   pRes     :PVariant;

begin
     Result :=NULL;
     scripter :=nil;
     try
        //Try with My Methods
        scripter :=TScriptableObject.Create(Self, false);
        pRes :=scripter.CallMethod(scripter.NameToDispID(MethodName), Args, NeedResult);
        if (pRes<>nil)
        then Result :=pRes^;
        scripter.Free;
     except
        scripter.Free;
        Result :=NULL;
        try
           //Try with InstanceObj Methods
           scripter :=TScriptableObject.Create(rInstanceObj, false);
           pRes :=scripter.CallMethod(scripter.NameToDispID(MethodName), Args, NeedResult);
           if (pRes<>nil)
           then Result :=pRes^;
           scripter.Free;
        except
           scripter.Free;
           Result :=NULL;
        end;
     end;
end;

function TScriptObject.GetPublicPropInfo(Name :String) :PPropInfo;
Var
   _parent :TScriptObjectClass;
   PubProClass :TClass;

begin
     _parent :=TScriptObjectClass(Self.ClassType);

     repeat
          PubProClass :=_parent.GetPublicPropertyAccessClass;
          if (PubProClass<>nil)
          then Result :=TypInfo.GetPropInfo(PubProClass, Name, tkProperties)
          else Result :=nil;
          
          if (Result=nil)
          then _parent := TScriptObjectClass(_parent.ClassParent);
     //IF the Property is not public in this Class, try in ancestors.
     // This method avoid to redeclare property as published in every TXXXAccess class,
     // for example :
     //    TLuaControl <- TLuaWinControl
     //    TControl    <- TWinControl
     // Without this method, in TLuaWinControl, you might declare every public property
     // of TWinControl including every public property of TControl.
     // With this method, in TLuaWinControl, you can declare only public property of TWinControl
     until (_parent=TObjectDispatch) or (Result<>nil);
end;

(*
begin
     Result :=TypInfo.GetPropInfo(GetPublicPropertyAccessClass, Name, tkProperties);
end;
*)

function TScriptObject.GetPublicPropValue(Name :String; AsString :Boolean) :Variant;
Var
   PropInfo :PPropInfo;

begin
     PropInfo :=GetPublicPropInfo(Name);
     if (PropInfo<>nil)
     then Result :=GetPropValue(rInstanceObj, PropInfo, AsString);
end;

procedure TScriptObject.SetPublicPropValue(Name :String; Value :Variant);

Var
   PropInfo :PPropInfo;

begin
     PropInfo :=GetPublicPropInfo(Name);
     if (PropInfo<>nil)
     then MySetPropValue(rInstanceObj, PropInfo, Value);
end;

function TScriptObject.GetMethodInfos(MethodName: String;
  var MethodInfo: PMethodInfoHeader; var ReturnInfo: PReturnInfo;
  var ParamInfos: TParamInfos): Boolean;
Var
   scripter :TScriptableObject;
   pRes     :PVariant;

begin
     Result :=false;
     scripter :=nil;
     try
        //Try with My Methods
        scripter :=TScriptableObject.Create(Self, false);
        Result :=scripter.GetMethodInfos(MethodName, MethodInfo, ReturnInfo, ParamInfos);
        if not(Result)
        then begin
                  scripter.Free;
                  //Try with InstanceObj Methods
                  scripter :=TScriptableObject.Create(rInstanceObj, false);
                  Result :=scripter.GetMethodInfos(MethodName, MethodInfo, ReturnInfo, ParamInfos);
                  scripter.Free;
             end;
     except
        scripter.Free;
        Result :=false;
     end;
end;


function TScriptObject.GetElementType(Name: String): PTypeInfo;
begin
     Result :=nil;
end;

procedure TScriptObject.SetArrayProp(Name: String; index, Value: Variant);
begin
end;

class function TScriptObject.GetPublicPropertyAccessClass: TClass;
begin
     Result :=nil;
end;

function TScriptObject.GetDefaultArrayProp: String;
begin
     Result :='';
end;

function TScriptObject.GetElement(Name: String): Variant;
begin
     Result :=NULL;
end;

function TScriptObject.GetArrayPropType(Name: String;
  index: Variant): PTypeInfo;
begin
     Result :=nil;
end;

procedure TScriptObject.SetElement(Name: String; Value: Variant);
begin
end;

function TScriptObject.GetArrayProp(Name: String; index: Variant): Variant;
begin
     Result :=NULL;
end;

//==============================================================================
//
function TScriptClassesList.allocData :Pointer;
begin
     GetMem(Result, sizeof(TScriptClassesListData));
     fillchar(Result^, sizeof(TScriptClassesListData), 0);
end;

procedure TScriptClassesList.deallocData(pData :Pointer);
begin
     FreeMem(pData, sizeof(TScriptClassesListData));
end;

function TScriptClassesList.Add(ObjClass :TClass; LuaClass :TScriptObjectClass=nil) :PScriptClassesListData;
begin
     Result :=internalFind(ObjClass.ClassName);
     if (Result=nil)
     then Result :=Self.Add;

     if (Result<>nil)
     then begin
               Result^.ObjClass :=ObjClass;
               (*if (LuaClass=nil)
               then Result^.LuaClass :=TScriptObject
               else*) Result^.LuaClass :=LuaClass;
          end;
end;

function TScriptClassesList.FindAncestor(ObjClass :TClass) :TScriptObjectClass;
Var
   _parent :TClass;
   Data    :PScriptClassesListData;

begin
     _parent :=ObjClass.ClassParent;
     Data :=nil;
     while (_parent<>nil) and (Data=nil) do
     begin
          Data :=internalFind(_parent);
          if (Data<>nil)
          then Result :=Data^.LuaClass
          else _parent := _parent.ClassParent;
     end;
     if (Data=nil)
     then Result :=TScriptObject;
end;

function TScriptClassesList.internalFind(ObjClassName :String) :PScriptClassesListData;

   function CompByClassName(Tag :Integer; ptData1, ptData2 :Pointer) :Boolean;
   begin
     Result := String(PChar(ptData1)) = Uppercase(PScriptClassesListData(ptData2)^.ObjClass.Classname);
   end;

begin
     Result :=Self.ExtFind(PChar(Uppercase(ObjClassName)), 0, @CompByClassName);
end;

function TScriptClassesList.internalFind(ObjClass :TClass) :PScriptClassesListData;

   function CompByClass(Tag :Integer; ptData1, ptData2 :Pointer) :Boolean;
   begin
     Result := TClass(ptData1) = PScriptClassesListData(ptData2)^.ObjClass;
   end;

begin
     Result :=Self.ExtFind(ObjClass, 0, @CompByClass);
end;

function TScriptClassesList.Find(ObjClass :TClass) :PScriptClassesListData;

begin
     Result :=Self.internalFind(ObjClass);

     if (Result<>nil)
     then begin
               if (Result^.LuaClass=nil) //The Class is registered, but have no LuaClass,
                                         //try to find a registered ancestor class
               then Result^.LuaClass :=FindAncestor(Result^.ObjClass);
          end
     else begin
               //The Class is not registered, try to find a registered ancestor class
               InternalData.ObjClass :=ObjClass;
               InternalData.LuaClass :=FindAncestor(ObjClass);

               if (InternalData.LuaClass<>nil)
               then Result :=@InternalData
               else Result :=nil;
          end;
end;


function TScriptClassesList.Find(ObjClassName :String) :PScriptClassesListData;
begin
     Result :=Self.internalFind(ObjClassName);

     if (Result<>nil)
     then begin
               if (Result^.LuaClass=nil) //The Class is registered, but have no LuaClass,
                                         //try to find a registered ancestor class
               then Result^.LuaClass :=FindAncestor(Result^.ObjClass);
          end
     else begin
               try
                  Result :=Self.internalFind(FindClass(ObjClassName));
               except
                  Result :=nil;
               end;
               if (Result<>nil)
               then begin
                         if (Result^.LuaClass=nil) //The Class is registered in VCL, but have no LuaClass,
                                                   //try to find a registered ancestor class
                         then Result^.LuaClass :=FindAncestor(Result^.ObjClass);
                    end
          end;
end;

//==============================================================================
//
function TScriptExistingObjList.allocData :Pointer;
begin
     GetMem(Result, sizeof(TScriptExistingObjListData));
     fillchar(Result^, sizeof(TScriptExistingObjListData), 0);
end;

procedure TScriptExistingObjList.deallocData(pData :Pointer);
begin
     PScriptExistingObjListData(pData)^.Name :='';
     FreeMem(pData, sizeof(TScriptExistingObjListData));
end;

function TScriptExistingObjList.Add(Obj :TObject; Name :String) :PScriptExistingObjListData;
begin
     Result :=Find(Name);
     if (Result=nil)
     then Result :=Self.Add;

     if (Result<>nil)
     then begin
               Result^.Obj :=Obj;
               Result^.Name :=Uppercase(Name);
          end;
end;

function TScriptExistingObjList.Find(Name :String) :PScriptExistingObjListData;

function CompByClass(Tag :Integer; ptData1, ptData2 :Pointer) :Boolean;
begin
     Result := String(PChar(ptData1)) = PScriptExistingObjListData(ptData2)^.Name;
end;

begin
     Result :=Self.ExtFind(PChar(Uppercase(Name)), 0, @CompByClass);
end;

function SetToString(TypeInfo: PTypeInfo; Value: Integer; Brackets: Boolean): string;
var
  S: TIntegerSet;
  I: Integer;
  EnumInfo :PTypeInfo;

begin
  Result := '';
  Integer(S) := Value;
  EnumInfo := GetTypeData(TypeInfo)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(EnumInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

  // grab the next enum name
function SetNextWord(var P: PChar): string;
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

function StringToSet(EnumInfo: PTypeInfo; const Value: string): Integer;
var
  P: PChar;
  EnumName: string;
  EnumValue: Longint;

begin
  Result := 0;
  if Value = '' then Exit;
  P := PChar(Value);

  // skip leading bracket and whitespace
  while P^ in ['[',' '] do
    Inc(P);

  EnumName := SetNextWord(P);
  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
      raise EPropertyConvertError.CreateFmt('Invalid Property Element %s', [EnumName]);
    Include(TIntegerSet(Result), EnumValue);
    EnumName := SetNextWord(P);
  end;
end;


procedure RegisterClass(ObjClass :TClass; LuaClass :TScriptObjectClass=nil);
begin
     ScriptClassesList.Add(ObjClass, LuaClass);
end;

procedure RegisterObject(Obj :TObject; Name :String; LuaClass :TScriptObjectClass=nil);
begin
     ScriptExistingObjList.Add(Obj, Name);
     ScriptClassesList.Add(Obj.ClassType, LuaClass);
end;

initialization
   ScriptClassesList :=TScriptClassesList.Create;
   ScriptExistingObjList :=TScriptExistingObjList.Create;

finalization
   ScriptClassesList.Free;
   ScriptExistingObjList.Free;

end.
