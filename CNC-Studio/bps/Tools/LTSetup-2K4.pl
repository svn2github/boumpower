// Support Pathes definitions list
// Copyright (c) 2001-2003 TM-CAD Engineering Torsten Moses
//
// This file defines additional Support pathes for AutoCAD LT 2004
// and is used by LTSetup.exe / LTUninst.exe; Remove this file, 
// if the application does not need support pathes and menu.
//
// SYNTAX :
// --------------
// ADDPATH # support  path to add
// SETTMPL # template path to set
// SETMENU # menufile menugroup popX [show]
//
// CURRENT PROGRAM PATH : may be defined by either '.', or '.\', or '' (empty)
// ANY RELATIVE PATHES  : must be defined with '.\pathname', this means, beginning with '.'
// ALL OTHER PATHES     : are treated to be absolute pathes (c:\pathname, \\server\pathname)
// MENUFILE : may include the pathname, same path definition rules like ADDPATH; extension is optional
// MENUGROUP : is the name of the menugroup defined by menufile
// POPX : is the first Popup-Identification defined by menufile with "***POPn", i.g. POP1
// SHOW : defines, wether the Popup is to be shown; if not given with 'show', the popup is not shown
//
// This file may also be created automatically by your installation software
// Pathes, that are not existing, will not be used as support pathes !
//
// Several pathes may be added by multiple 'ADDPATH #" entries
// Only ONE template path can be used with 'SETTMPL #" entry
// Only ONE menufile path can be used with 'SETMENU #" entry

// PLEASE NOTE : ALL ENTRIES ARE OPTIONALLY !

//------------- Sample Support Path Definitions ------------
//ADDPATH  # .\
//ADDPATH  # .\Supports
//ADDPATH  # f:\programme\Autodesk\AutoCAD
//ADDPATH  # \\Server\Volume1\

//SETTMPL # .\
//SETTMPL # .\Templates
//SETTMPL # f:\programs\MyApplication\Templates
//SETTMPL # \\Server\Volume1\Templates

//SETMENU # .\Support\AppMenu.mnu AppMenuGroup  Pop1  SHOW
