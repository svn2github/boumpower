//******************************************************************************
//***                     LUA SCRIPT FUNCTIONS                               ***
//***                                                                        ***
//***        (c) Massimo Magnano 2006                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : Script_Classes.pas   (rev. 1.0)
//
//  Description : Access from scripts to Class declared in "Classes.pas"
//
//******************************************************************************
unit Script_Classes;

interface

uses TypInfo, Script_System;

type
    TScriptComponent = class(TScriptObject)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;
       
    public
                                  //TClass         TComponent TComponent
       function ScriptCreate(ObjClass :Integer; AOwner :Integer) :Integer; overload;

       function GetArrayPropType(Name :String; index :Variant) :PTypeInfo; override;
       function GetArrayProp(Name :String; index :Variant) :Variant; override;
       function GetElementType(Name :String) :PTypeInfo; override;
       function GetElement(Name :String) :Variant; override;
    end;


implementation

uses Classes, SysUtils, StdCtrls, Variants;

type
    TComponentAccess = class(TComponent)
    published
       property ComponentIndex;
       property ComponentCount;
    end;


function TScriptComponent.ScriptCreate(ObjClass :Integer; AOwner :Integer) :Integer;
Var
   xObjClass :TClass;

begin
     xObjClass :=TClass(ObjClass);
     if (xObjClass.InheritsFrom(TComponent))
     then Result :=Integer(TComponentClass(xObjClass).Create(TComponent(AOwner)))
     else Result :=Integer(TComponent(ScriptCreate(ObjClass)));
end;

function TScriptComponent.GetArrayPropType(Name :String; index :Variant) :PTypeInfo;
begin
     Name :=Uppercase(Name);
     Result :=nil;

     if (Name='COMPONENTS')
     then begin
               if (TComponent(InstanceObj).Components[index]<>nil)
               then Result :=TypeInfo(TComponent)
               else Result :=nil;
          end;
end;

function TScriptComponent.GetArrayProp(Name :String; index :Variant) :Variant;
begin
     Name :=Uppercase(Name);
     Result :=NULL;

     if (Name='COMPONENTS')
     then begin
               if (TComponent(InstanceObj).Components[index]<>nil)
               then Result :=Integer(TComponent(InstanceObj).Components[index]);
          end;
end;

function TScriptComponent.GetElementType(Name :String) :PTypeInfo;
Var
   upName :String;

begin
     upName :=Uppercase(Name);
     Result :=nil;

     if (upName='COMPONENTS')
     then Result :=@TypeInfoArray
     else
     if (TComponent(InstanceObj).FindComponent(Name)<>nil)
     then Result :=TypeInfo(TComponent);
end;

function TScriptComponent.GetElement(Name :String) :Variant;
Var
   theComponent :TComponent;

begin
     Result :=NULL;

     theComponent :=TComponent(InstanceObj).FindComponent(Name);
     if (theComponent<>nil)
     then Result :=Integer(theComponent);
end;

class function TScriptComponent.GetPublicPropertyAccessClass :TClass;
begin
     Result :=TComponentAccess;
end;


initialization
   Script_System.RegisterClass(TComponent, TScriptComponent);

end.
