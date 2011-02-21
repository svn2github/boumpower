//******************************************************************************
//***                     SCRIPT VCL FUNCTIONS                               ***
//***                                                                        ***
//***        (c) Massimo Magnano 2006                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : Lua_Controls.pas   (rev. 1.0)
//
//  Description : Access from Lua scripts to Class declared in "Controls.pas"
//
//******************************************************************************
// TO-DO : Record like TRect, TPoint property how can i return in variant(lua)
unit Script_Controls;



interface

uses TypInfo, Script_Classes;

type
    TScriptControl = class(TScriptComponent)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;
    end;

    TScriptWinControl = class(TScriptControl)
    protected
       class function GetPublicPropertyAccessClass :TClass; override;

    public
       function GetArrayPropType(Name :String; index :Variant) :PTypeInfo; override;
       function GetArrayProp(Name :String; index :Variant) :Variant; override;
       function GetElementType(Name :String) :PTypeInfo; override;
    end;


implementation

uses Classes, Controls, SysUtils, Variants, Script_System;

type
    TControlAccess = class(TControl)
    published
       property Enabled;
       property Action;
       property Align;
       property Anchors;
       property BiDiMode;
       property BoundsRect;
       property ClientHeight;
       property ClientOrigin;
       property ClientRect;
       property ClientWidth;
       property Constraints;
       property ControlState;
       property ControlStyle;
       property DockOrientation;
       property Floating;
       property FloatingDockSiteClass;
       property HostDockSite;
       property LRDockWidth;
       property Parent;
       property ShowHint;
       property TBDockHeight;
       property UndockHeight;
       property UndockWidth;
       property Visible;
       //property WindowProc;   pericolosa.
    end;

    TWinControlAccess = class(TWinControl)
    published
       // See Comment on TScriptObject.GetPublicPropInfo, it explain why i have no need
       // to redeclare also the public property of TControl  
       property DockClientCount;
       property DockSite;
       property DockManager;
       property DoubleBuffered;
       property AlignDisabled;
       property VisibleDockClientCount;
       property Brush;
       property ControlCount;
       property Handle;
       property ParentWindow;
       property Showing;
       property TabOrder;
       property TabStop;
       property UseDockManager;
    end;


class function TScriptControl.GetPublicPropertyAccessClass :TClass;
begin
     Result :=TControlAccess;
end;

//==============================================================================
//
class function TScriptWinControl.GetPublicPropertyAccessClass :TClass;
begin
     Result :=TWinControlAccess;
end;

function TScriptWinControl.GetArrayPropType(Name :String; index :Variant) :PTypeInfo;
begin
     Name :=Uppercase(Name);
     Result :=nil;

     if (Name='CONTROLS')
     then begin
               if (TWinControl(InstanceObj).Controls[index]<>nil)
               then Result :=TypeInfo(TControl);
          end
     else
     if (Name='DOCKCLIENTS')
     then begin
               if (TWinControl(InstanceObj).DockClients[index]<>nil)
               then Result :=TypeInfo(TControl);
          end
     else
     Result :=inherited GetArrayPropType(Name, index);
end;

function TScriptWinControl.GetArrayProp(Name :String; index :Variant) :Variant;
begin
     Name :=Uppercase(Name);

     if (Name='CONTROLS')
     then begin
               if (TWinControl(InstanceObj).Controls[index]<>nil)
               then Result :=Integer(TWinControl(InstanceObj).Controls[index])
               else Result :=NULL;
          end
     else
     if (Name='DOCKCLIENTS')
     then begin
               if (TWinControl(InstanceObj).DockClients[index]<>nil)
               then Result :=Integer(TWinControl(InstanceObj).DockClients[index])
               else Result :=NULL;
          end
     else
     Result := inherited GetArrayProp(name, index)
end;

function TScriptWinControl.GetElementType(Name :String) :PTypeInfo;
Var
   upName :String;

begin
     upName :=Uppercase(Name);

     if (upName='CONTROLS') or (upName='DOCKCLIENTS')
     then Result :=@TypeInfoArray
     else Result :=inherited GetElementType(Name);
end;

initialization
   Script_System.RegisterClass(TControl, TScriptControl);
   Script_System.RegisterClass(TWinControl, TScriptWinControl);

end.
