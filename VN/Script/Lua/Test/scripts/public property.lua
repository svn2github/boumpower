-- Test of public property (not published) 

F = GetObject('Form1');
X =F.Button1.Parent.Name;
F.Label1.Caption = X;