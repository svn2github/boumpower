var WshShell = WScript.CreateObject ("WScript.Shell");
var ThisDirectory = WshShell.CurrentDirectory;
var fso = new ActiveXObject("Scripting.FileSystemObject");
var CmdParameters = "";
var objArgs = WScript.Arguments;
var RetCode = 0;

// Get command line parameters
for (i = 0; i < objArgs.length; i++)
{
   CmdParameters += " "+objArgs(i);
}

//WScript.Echo (CmdParameters);

VisitFolders(ThisDirectory, true);
WshShell.CurrentDirectory = ThisDirectory;	// make sure that we havn't change directory

WScript.Quit(RetCode); 

//-----------------------------------------------------------------------------------------------------------------------------

function FolderVisitAction(path) {
    var ret = 0;
    
    WshShell.CurrentDirectory = path;
    if (fso.FileExists("svncypher.js")) {
	ret = WshShell.Run("cmd /C WScript svncypher.js"+CmdParameters, 7, true);
	if (ret != 0) RetCode = ret;
    }
}

function VisitFolders(path, Recursive) {
    var folder = fso.GetFolder(path+"\\");
    var subFlds = new Enumerator(folder.SubFolders);
    var f;

    FolderVisitAction(path);	// apply to this Folder
    for (; !subFlds.atEnd(); subFlds.moveNext()) {
	f = subFlds.item(); 
	if (!(f.Attributes & 2)) {	// skip hidden folders
          if (Recursive) 
		VisitFolders(f, Recursive)
	  else
	  	FolderVisitAction(f);
        }
    }
}
