// Support Pathes definitions list
// Copyright (c) 2001-2003 TM-CAD Engineering Torsten Moses
//
// This file defines additional Support pathes for AutoCAD and is
// used by Acad_Reg.exe; Remove this file, if the application does
// not need support pathes
//
// SYNTAX :
// ADDPATH # support  path to add
// SETTMPL # template path to set
//
// CURRENT PROGRAM PATH : may be defined by either '.', or '.\', or '' (empty)
// ANY RELATIVE PATHES  : must be defined with '.\pathname', this means, beginning with '.'
// ALL OTHER PATHES     : are treated to be absolute pathes (c:\pathname, \\server\pathname)
//
// This file may also be created automatically by your installation software
// Pathes, that are not existing, will not be used as support pathes !
//
// Several pathes may be added by multiple 'ADDPATH #" entries
// Only ONE template path can be used with 'SETTMPL #" entry

//------------------- Support Path definitions--------------
//ADDPATH # .\
//ADDPATH # .\Supports
//ADDPATH # f:\programs\Autodesk\AutoCAD
//ADDPATH # \\Server\Volume1\

//SETTMPL # .\
//SETTMPL # .\Templates
//SETTMPL # f:\programs\MyApplication\Templates
//SETTMPL # \\Server\Volume1\Te,plates
