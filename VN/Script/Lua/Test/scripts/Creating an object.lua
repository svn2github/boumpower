-- Test of Creating an object and setting a public property

F = GetObject('Form1');
X = CreateObject('TLabel', false, F);
X.Name ='New_Label'
X.Parent = F.Panel1;
X.Left = 20;
X.Top = 30;

