VERSION 5.00
Begin VB.UserControl DockBars 
   ClientHeight    =   4260
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4335
   ScaleHeight     =   4260
   ScaleWidth      =   4335
   Begin VB.Frame Frame1 
      Caption         =   "Dock Container Collection Tests"
      Height          =   3975
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4095
      Begin VB.CommandButton Command2 
         Caption         =   "Update List"
         Height          =   375
         Left            =   240
         TabIndex        =   4
         Top             =   1440
         Width           =   3615
      End
      Begin VB.TextBox Text1 
         Height          =   375
         Left            =   240
         TabIndex        =   3
         Top             =   360
         Width           =   3615
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Add"
         Height          =   375
         Left            =   240
         TabIndex        =   2
         Top             =   840
         Width           =   3615
      End
      Begin VB.ListBox List1 
         Height          =   1815
         Left            =   240
         TabIndex        =   1
         Top             =   1920
         Width           =   3615
      End
   End
End
Attribute VB_Name = "DockBars"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Implements IRetrieveDockableContainer2

Private Sub IRetrieveDockableContainer2_SetDockContainer(ByVal newVal As AC_CONT.IAcadDockableContainer2)
    newVal.EnableDockPositions = acDockLeft + acDockRight
    
    Dim rect(0 To 3) As Integer
    rect(0) = 75    ' left
    rect(1) = 75    ' top
    rect(2) = 375   ' right
    rect(3) = 375   ' bottom
    
    newVal.SetPreferredDockPosition acFloating, rect
    Command2_Click
End Sub

Private Sub UserControl_Resize()
    Frame1.Top = 0
    Frame1.Left = 5
    Frame1.Height = UserControl.Height - 5
    Frame1.Width = UserControl.Width - 5
End Sub


Private Sub Command1_Click()
    Dim DockBars As New AcadDockableContainers
    DockBars.Add Text1.Text
    Command2_Click
End Sub

Private Sub Command2_Click()
    Dim DockBars As New AcadDockableContainers
    Dim DockBar As AcadDockableContainer2
    
    List1.Clear
    For Each DockBar In DockBars
        List1.AddItem DockBar.Name
    Next
End Sub

