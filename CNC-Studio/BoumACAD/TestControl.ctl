VERSION 5.00
Begin VB.UserControl TestControl 
   ClientHeight    =   4125
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   3675
   ScaleHeight     =   4125
   ScaleWidth      =   3675
   Begin VB.Frame Frame1 
      Caption         =   "Dock Container Tests"
      Height          =   3855
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   3375
      Begin VB.CommandButton Command5 
         Caption         =   "Destroy"
         Height          =   495
         Left            =   1680
         TabIndex        =   8
         Top             =   3120
         Width           =   1215
      End
      Begin VB.CommandButton Command4 
         Caption         =   "Hide / Show"
         Height          =   495
         Left            =   240
         TabIndex        =   7
         Top             =   3120
         Width           =   1215
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Allow Docking"
         Height          =   255
         Left            =   840
         TabIndex        =   6
         Top             =   2640
         Value           =   1  'Checked
         Width           =   1575
      End
      Begin VB.CommandButton Command3 
         Caption         =   "Float"
         Height          =   495
         Left            =   1680
         TabIndex        =   5
         Top             =   1920
         Width           =   1215
      End
      Begin VB.CommandButton Command2 
         Caption         =   "Dock"
         Height          =   495
         Left            =   240
         TabIndex        =   4
         Top             =   1920
         Width           =   1215
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Set New Caption"
         Height          =   495
         Left            =   960
         TabIndex        =   3
         Top             =   960
         Width           =   2175
      End
      Begin VB.TextBox Text1 
         Height          =   375
         Left            =   960
         TabIndex        =   2
         Top             =   360
         Width           =   2175
      End
      Begin VB.Label Label1 
         Caption         =   "Caption:"
         Height          =   255
         Left            =   240
         TabIndex        =   1
         Top             =   360
         Width           =   735
      End
   End
End
Attribute VB_Name = "TestControl"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Dim dockContainer As AcadDockableContainer2
Implements IRetrieveDockableContainer2

Private Sub IRetrieveDockableContainer2_SetDockContainer(ByVal newVal As AC_CONT.IAcadDockableContainer2)
    Set dockContainer = newVal
    dockContainer.EnableDockPositions = acDockLeft + acDockRight
    
    Dim rect(0 To 3) As Integer
    rect(0) = 50    ' left
    rect(1) = 50    ' top
    rect(2) = 300   ' right
    rect(3) = 350   ' bottom
    
    dockContainer.SetPreferredDockPosition acFloating, rect
End Sub

Private Sub UserControl_Resize()
    Frame1.Top = 0
    Frame1.Left = 5
    Frame1.Height = UserControl.Height - 5
    Frame1.Width = UserControl.Width - 5
End Sub

Private Sub UserControl_Show()
    Text1.Text = dockContainer.Caption
    
    If dockContainer.AllowDocking Then
        Check1.Value = 1
    Else
        Check1.Value = 0
    End If
End Sub

Private Sub Command1_Click()
    dockContainer.Caption = Text1.Text
End Sub

Private Sub Command2_Click()
    Call dockContainer.Dock
End Sub

Private Sub Command3_Click()
    Call dockContainer.Float
End Sub

Private Sub Check1_Click()
    If Check1.Value = 0 Then
        dockContainer.AllowDocking = False
    Else
        dockContainer.AllowDocking = True
    End If
End Sub

Private Sub Command4_Click()
    dockContainer.Hide
    MsgBox "Container window is now hidden, press 'OK' to redisplay it"
    dockContainer.Show
End Sub

Private Sub Command5_Click()
    dockContainer.Destroy
End Sub

