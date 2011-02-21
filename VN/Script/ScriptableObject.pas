//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***                    (c) Massimo Magnano 24-10-2006.                     ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : ScriptableObject.pas      REV. 0.3   (01-12-2006)
//
//  Description : Delphi scripting support
//
//  Rev. 0.3
//          MaxM Adds :
//                     LexicalOrder Call for a Method
//                     Infos About a Method
//
//  Rev. 0.2
//          MaxM Adds :
//                     Reorganization of the Unit
//
//
//The Original Code is: JvOle2Auto.PAS, released on 2002-07-04.
//
//The Initial Developers of the Original Code are:
//  Fedor Koshevnikov, Igor Pavluk and Serge Korolev
//     Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
//     Copyright (c) 2001,2002 SGB Software
//All Rights Reserved.

unit ScriptableObject;

interface

{$J+}
{$O-}

uses Windows, ActiveX, ObjComAuto, SysUtils, TypInfo, ObjAuto;

type
    TParamInfos= array of PParamInfo;

{$METHODINFO ON}
//{$TYPEINFO ON} use this if you want only published methods
  TScriptableObject = class(TObjectDispatch)
  private
   FRetValue : Variant;
   FInstanceObj : TObject;

  public
    constructor Create(Instance: TObject; Owned: Boolean = True);


    function NameToDispID(const AName: string): TDispID;
    function Invoke2(dispidMember: TDispID; wFlags: Word;
                     var pdispparams: TDispParams; Res: PVariant): PVariant;
    function GetPropertyByID(ID: TDispID): PVariant;
    function GetProperty(PropName :String): PVariant;
    function GetPropertyTypeInfoByID(ID: TDispID): PTypeInfo;
    function GetPropertyTypeInfo(PropName :String): PTypeInfo;
    procedure SetPropertyByID(ID: TDispID; const Prop: array of const);
    procedure SetProperty(PropName :String; const Prop: array of const);
    function CallMethod(ID: TDispID; const Args : array of variant;
                        NeedResult: Boolean): PVariant; overload;
    function CallMethod(MethodName :String; const Args : array of variant;
                        NeedResult: Boolean; ResultTypeInfo :PReturnInfo): PVariant; overload;

    function CallMethodLexicalOrder(ID: TDispID; const Args : array of variant;
                        NeedResult: Boolean): PVariant; overload;
    function CallMethodLexicalOrder(MethodName :String; const Args : array of variant;
                        NeedResult: Boolean; ResultTypeInfo :PReturnInfo): PVariant; overload;
    function GetMethodInfos(MethodName :String;
                       var MethodInfo: PMethodInfoHeader; var ReturnInfo: PReturnInfo;
                       var ParamInfos: TParamInfos):Boolean;



    property InstanceObj : TObject read FInstanceObj write FInstanceObj;
  end;
//{$TYPEINFO OFF}
{$METHODINFO OFF}


implementation

type
    PPParamInfo = ^PParamInfo;

constructor TScriptableObject.Create(Instance: TObject; Owned: Boolean = True);
begin
     if (Instance=nil)
     then inherited Create(Self, false)
     else inherited Create(Instance, Owned);

     InstanceObj :=Instance;
end;

function TScriptableObject.NameToDispID(const AName: string): TDispID;
var
  CharBuf: array [0..255] of WideChar;
  P: array [0..0] of PWideChar;
begin
  StringToWideChar(AName, @CharBuf[0], 256);
  P[0] := @CharBuf[0];
  GetIDsOfNames(GUID_NULL, @P,  1, GetThreadLocale, @Result);
end;


function TScriptableObject.Invoke2(dispidMember: TDispID; wFlags: Word;
      var pdispparams: TDispParams; Res: PVariant): PVariant;
var
  pexcepinfo: TExcepInfo;
  puArgErr: Integer;
begin
  if Res <> nil then VarClear(Res^);
  try
    Invoke(dispidMember, GUID_NULL, GetThreadLocale, wFlags, pdispparams, Res, @pexcepinfo, @puArgErr);
  except
    if Res <> nil then VarClear(Res^);
    Result := Res;
    raise;
  end;
end;

function TScriptableObject.GetPropertyByID(ID: TDispID): PVariant;
const
  Disp: TDispParams = (rgvarg: nil; rgdispidNamedArgs: nil;
    cArgs: 0; cNamedArgs: 0);
begin
  Result := Invoke2(ID, DISPATCH_PROPERTYGET, Disp, @FRetValue);
end;

function TScriptableObject.GetProperty(PropName :String): PVariant;
begin
     Result :=GetPropertyByID(NameToDispID(PropName));
end;

procedure AssignVariant(var Dest: TVariantArg;   const Value: TVarRec);
begin
    with Value do
      case VType of
        vtInteger:
          begin
            Dest.vt := VT_I4;
            Dest.lVal := VInteger;
          end;
        vtBoolean:
          begin
            Dest.vt := VT_BOOL;
            Dest.vbool := VBoolean;
          end;
        vtChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(VChar);
          end;
        vtExtended:
          begin
            Dest.vt := VT_R8;
            Dest.dblVal := VExtended^;
          end;
        vtString:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(VString^);
          end;
        vtPointer:
          if VPointer = nil then begin
            Dest.vt := VT_NULL;
            Dest.byRef := nil;
          end
          else begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VPointer;
          end;
        vtPChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(StrPas(VPChar));
          end;
        vtObject:
          begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VObject;
          end;
        vtClass:
          begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VClass;
          end;
        vtWideChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := @VWideChar;
          end;
        vtPWideChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := VPWideChar;
          end;
        vtAnsiString:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(string(VAnsiString));
          end;
        vtCurrency:
          begin
            Dest.vt := VT_CY;
            Dest.cyVal := VCurrency^;
          end;
        vtVariant:
          begin
            Dest.vt := VT_BYREF or VT_VARIANT;
            Dest.pvarVal := VVariant;
          end;
        vtInterface:
          begin
            Dest.vt := VT_UNKNOWN or VT_BYREF;
            Dest.byRef := VInterface;
          end;
        vtInt64:
          begin
            Dest.vt := VT_I8 or VT_BYREF;
            Dest.byRef := VInt64;
          end;
      end;
end;


procedure TScriptableObject.SetPropertyByID(ID: TDispID; const Prop: array of const);
const
  NameArg: TDispID = DISPID_PROPERTYPUT;
var
  Disp: TDispParams;
  ArgCnt, I: Integer;
  Args: array[0..63] of TVariantArg;
begin
  ArgCnt := 0;
  try
    for I := 0 to High(Prop) do begin
      AssignVariant(Args[I], Prop[I]);
      Inc(ArgCnt);
      if ArgCnt >= 64 then Break;
    end;
    with Disp do begin
      rgvarg := @Args;
      rgdispidNamedArgs := @NameArg;
      cArgs := ArgCnt;
      cNamedArgs := 1;
    end;
    Invoke2(ID, DISPATCH_PROPERTYPUT, Disp, nil);
  finally
  end;
end;

procedure TScriptableObject.SetProperty(PropName :String; const Prop: array of const);
begin
     SetPropertyByID(NameToDispID(PropName), Prop);
end;

function WashVariant(const Value: Variant): OleVariant;
begin
  if TVarData(Value).VType = (varString or varByRef) then
    Result := PString(TVarData(VAlue).VString)^ + ''
  else
    Result := Value;
end;

function TScriptableObject.CallMethod(ID: TDispID; const Args : array of variant;
  NeedResult: Boolean): PVariant;
var
  DispParams: TDispParams;
  I: Integer;
  OleArgs: array of OleVariant;

begin
  if ID=-1
  then raise Exception.Create('Method not found');

  SetLength(OleArgs, High(Args) + 1);
  for I := Low(Args) to High(Args) do
    OleArgs[I] := WashVariant(Args[I]);
  DispParams.rgvarg := @OleArgs[0];
  DispParams.cArgs := High(Args) + 1;
  DispParams.rgdispidNamedArgs := nil;
  DispParams.cNamedArgs := 0;
  if NeedResult
  then Result := Invoke2(ID, DISPATCH_METHOD or DISPATCH_PROPERTYGET, DispParams, @FRetValue)
  else begin
            Result :=nil;
            Invoke2(ID, DISPATCH_METHOD or DISPATCH_PROPERTYGET, DispParams, nil);
       end;
end;

function TScriptableObject.CallMethod(MethodName :String; const Args : array of variant;
    NeedResult: Boolean; ResultTypeInfo: PReturnInfo): PVariant;
Var
   MethodInfo: PMethodInfoHeader;

begin
  if NeedResult
  then begin
            MethodInfo := ObjAuto.GetMethodInfo(InstanceObj, MethodName);
            ResultTypeInfo := PReturnInfo(MethodInfo);
            Inc(Integer(ResultTypeInfo), SizeOf(TMethodInfoHeader) - SizeOf(ShortString) + 1 +
                Length(MethodName));
       end
  else ResultTypeInfo :=nil;

  Result :=CallMethod(NameToDispID(MethodName), Args, NeedResult);
end;

function TScriptableObject.GetPropertyTypeInfo(PropName: String): PTypeInfo;
begin
     Result :=GetPropertyTypeInfoByID(NameToDispID(PropName));
end;

function TScriptableObject.GetPropertyTypeInfoByID(ID: TDispID): PTypeInfo;
begin
     Result :=nil;
     GetTypeInfo(ID, GetThreadLocale, Result);
end;

function TScriptableObject.GetMethodInfos(MethodName: String;
  var MethodInfo: PMethodInfoHeader; var ReturnInfo: PReturnInfo;
  var ParamInfos: TParamInfos): Boolean;
var
  Name: ShortString;
  InfoEnd: Pointer;
  Params, Param: PParamInfo;
  I: Integer;
  ID: Cardinal;
  CompIndex: Integer;
  Instance: TObject;
  pt      :Pointer;

(*  CharBuf: array [0..255] of WideChar;
  P: array [0..0] of PWideChar;
*)
begin
(*  StringToWideChar(MethodName, @CharBuf[0], 256);
  P[0] := @CharBuf[0];
  GetIDsOfNames(GUID_NULL, @P,  1, GetThreadLocale, @Result);

  AllocDispID(dkMethod, Info, Instance);
*)

  SetLength(ParamInfos, 0);
  ReturnInfo := nil;
  MethodInfo := ObjAuto.GetMethodInfo(InstanceObj, MethodName);
  if (MethodInfo<>nil) then
  begin
       pt := MethodInfo;
       MethodName := PMethodInfoHeader(pt)^.Name;
       Inc(Integer(pt), SizeOf(TMethodInfoHeader) - SizeOf(ShortString) + 1 +
           Length(MethodName));
       ReturnInfo := pt;
       Inc(Integer(pt), SizeOf(TReturnInfo));

       Param := PParamInfo(pt);
       InfoEnd := Pointer(Integer(MethodInfo) + MethodInfo^.Len);
       ID := 0;
       while Integer(Param) < Integer(InfoEnd) do
       begin
            // ignore Self
            if not((Param^.ParamType^.Kind = tkClass) and SameText(Param^.Name, 'SELF'))
            then begin
                      SetLength(ParamInfos, ID+1);
                      ParamInfos[ID] := Param;
                      Inc(ID);
                 end;

            Inc(Integer(Param), SizeOf(TParamInfo) - SizeOf(ShortString) + 1 +
                Length(PParamInfo(Param)^.Name));
       end;
  end;
end;

function TScriptableObject.CallMethodLexicalOrder(MethodName: String;
  const Args: array of variant; NeedResult: Boolean;
  ResultTypeInfo: PReturnInfo): PVariant;
begin
     Result :=CallMethodLexicalOrder(NameToDispID(MethodName), Args, NeedResult);
end;

function TScriptableObject.CallMethodLexicalOrder(ID: TDispID;
  const Args: array of variant; NeedResult: Boolean): PVariant;
begin

end;

initialization
   OleInitialize(nil);
finalization
   OleUninitialize;

end.
