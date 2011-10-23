// Usage :
// Start_Commit Hook  WScrip "Path to this script" /c "PassPhrase"
// Post_update  Hook  WScrip "Path to this script" /d "PassPhrase"

gnuPG = "\"c:\\Program files\\GNU\\gnuPG\\gpg.exe\"";		// Path of gnuPG
CypherLog = "svncypher.log";					// Log file name

var FSO = WScript.CreateObject("Scripting.FileSystemObject");
var oShell = WScript.CreateObject("WScript.Shell");		// Shell object
var message = "Cypher script for Remote Control Embedded Device -> Windows\n\n";
var RetCode = 0;						// Script return code
var Ret;


args = WScript.Arguments.length;

if (args < 2) 
{
  	WScript.Echo("!!! usage: svncypher [/c, /d] \"passphrase\" ");
	WScript.Quit(-1);
};

mode = WScript.Arguments.Item(0);		// /c = crypt for pre-commit hook, /d = decrypt for post-commit hook
passphrase = WScript.Arguments.Item(1);


if (mode == "/c")
{ // put here action to do for /c (crypt) argument
	if (MoreRecent("RemoteWidgets.pas", CypherLog)) {
	  FSO.CopyFile("Release\\Win32\\RemoteWidgets.dcu", "RemoteWidgets.dcu");
	  message += "File copied      : Release\\Win32\\RemoteWidgets.dcu\n";
	  Ret = crypt("RemoteWidgets.pas", passphrase);
	  if (Ret != 0) 
		RetCode = Ret
  	  else {
		AddToCypherLog("Crypted RemoteWidgets.pas");		
		message += "File crypted     : RemoteWidgets.pas with "+passphrase+"\n";
	       }
	}
}

if (mode == "/d")
{ // put here action to do for /d (decrypt) argument
	if (MoreRecent("RemoteWidgets.pas.asc", CypherLog)) {
	  FSO.CopyFile("RemoteWidgets.pas", "RemoteWidgets.pas.old");
	  message += "File copied      : RemoteWidgets.pas.old\n";
  	  Ret = decrypt("RemoteWidgets.pas", passphrase);
	  if (Ret != 0) {
	  	RetCode = Ret;
		message += "ERROR, check RemoteWidgets.pas.old to restore"; 
	    }
	  else {
		AddToCypherLog("Decrypted RemoteWidgets.pas");		
		message += "File decrypted : RemoteWidgets.pas with "+passphrase+"\n";
		FSO.DeleteFile("RemoteWidgets.dcu");
		message += "File deleted     : RemoteWidgets.dcu";
	    }
	}
}

if (RetCode != 0) message += "\n ---> ERROR : RetCode = "+RetCode;

WScript.Echo(message);
WScript.Quit(RetCode);

//--------------------------------------------------------------------------------------------------------------------------------


function MoreRecent(ThisFile, ThanFile)
{
	if (!FSO.FileExists(ThisFile)) return(false);
	if (!FSO.FileExists(ThanFile)) return(true);
	var This = FSO.GetFile(ThisFile);
	var Than = FSO.GetFile(ThanFile);
	return(This.DateLastModified > Than.DateLastModified);
}

function AddToCypherLog(msg)
{
	var dt = new Date();
	var f = FSO.OpenTextFile(CypherLog, 8, true); // Touch the file for date
	f.WriteLine(dt.toString()+" "+msg);
	f.Close();
}

function crypt(filename, passphrase) 
{
//	WScript.Echo("Crypt " + filename + " PassPhrase = " + passphrase);
 	ret = oShell.Run(gnuPG + " --batch --yes --passphrase \"" + passphrase +"\" --armor --symmetric \"" + filename +"\"", 9, true);
	return(ret);
};

function decrypt(filename, passphrase) 
{
//	WScript.Echo("Decrypt " + filename + " PassPhrase = " + passphrase);
	ret = oShell.Run(gnuPG + " --batch --yes --passphrase \"" + passphrase +"\" -d -o \"" + filename +"\" \"" + filename +".asc\"", 9, true);
//	var f = FSO.OpenTextFile(filename +".asc", 8, false); // Touch the file
//	f.WriteLine("\nDecrypted");
//	f.Close();
	return(ret);
}


