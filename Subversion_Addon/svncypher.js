WScript.Echo("Cypher script for subversion");

gnuPG = "\"c:\\Program files\\GNU\\gnuPG\\gpg.exe\"";	// Path of gnuPG
oShell = WScript.CreateObject("WScript.Shell");		// Shell object
RetCode = 0;						// Script return code

function crypt(filename, passphrase) 
{
	WScript.Echo("Crypt " + filename + " PassPhrase = " + passphrase);
 	ret = oShell.Run(gnuPG + " --batch --yes --passphrase \"" + passphrase +"\" --armor --symmetric \"" + filename +"\"", 9, true);
	return(ret);
};

function decrypt(filename, passphrase) 
{
	WScript.Echo("Decrypt " + filename + " PassPhrase = " + passphrase);
	oShell.Run("cmd /C copy \"" + filename + "\" \"" + filename + ".old\" /Y", 5, true);
	ret = oShell.Run(gnuPG + " --batch --yes --passphrase \"" + passphrase +"\" -d -o \"" + filename +"\" \"" + filename +".asc\"", 9, true);
	if (ret != 0) WScript.Echo("ERROR, check ", filename, ".old (RetCode=", ret, ")");
	return(ret);
}

function filecopy(sourcename, destname)
{
	WScript.Echo("Copy " + sourcename + " " + destname);
	oShell.Run("cmd /C copy \"" + sourcename + "\" \"" + destname + "\" /Y", 5, true);
	return(0);
};

args = WScript.Arguments.length;

if (args < 2) 
{
  	WScript.Echo("usage: svncypher [/c, /d] \"passphrase\" ");
	WScript.Quit(-1);
};

mode = WScript.Arguments.Item(0);		// /c = crypt for pre-commit hook, /d = decrypt for post-commit hook
passphrase = WScript.Arguments.Item(1);


if (mode == "/c")
{ // put here action to do for /c (crypt) argument
	filecopy("Test.txt", "");
	RetCode = crypt("Test.txt", passphrase);
}

if (mode == "/d")
{ // put here action to do for /d (decrypt) argument
	RetCode = decrypt("Test.txt", passphrase);
}

if (RetCode == 0) 
 	WScript.Echo("Cypher done.");
else 
 	WScript.Echo("Cypher ERROR : RetCode=", RetCode);

WScript.Quit(RetCode);

