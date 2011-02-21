-- Test of set and enumeration property

F = GetObject('Form1');
X = F.Panel1.Anchors + 'akRight';
F.Panel1.Anchors = X;
Y = F.Memo2.Anchors - 'akBottom';
F.Memo2.Anchors = Y;