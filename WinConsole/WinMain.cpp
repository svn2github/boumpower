#include <windows.h>
#include <streambuf>
#include <iostream>
#include "main.h"
#include "cindlg.h"

using namespace std;

/*  Declare Windows procedure  */
LRESULT CALLBACK WindowProcedure (HWND, UINT, WPARAM, LPARAM);
BOOL APIENTRY CinDlgProc(HWND, UINT, WPARAM, LPARAM);

/*  Make the class name into a global variable  */
char szClassName[ ] = "WinConsole";
char szCaption[ ] = "Console en fenêtre windows";

static HINSTANCE hinst;
static HWND hMainWnd;
static HMENU MainMenu; // 
static HMENU FileMenu; // 
static HWND hEdit;

#define ID_File 100
#define ID_Execute 101
#define ID_Exit 901

//-----------------------------------------------------------------------------
// http://www.devmaster.net/forums/showthread.php?t=7037

const int LineSize = 256;
static CHAR stCin[256];

class BufferedStringBuf : public std::streambuf
{
public:
    BufferedStringBuf(int bufferSize) 
    {
        if (bufferSize)
        {
            char *ptr = new char[bufferSize];
            setp(ptr, ptr + bufferSize);
            setg(ptr, ptr + bufferSize, ptr + bufferSize);
        }
        else
        {
            setp(0, 0);
            setg(0, 0, 0);
        }
    }
    virtual ~BufferedStringBuf() 
    {
        sync();
        delete[] pbase();
    }

    virtual void writeString(const std::string &str) = 0;
    virtual void readString(std::string &str) = 0;

private:
    int	overflow(int c)
    {
        sync();

        if (c != EOF)
        {
            if (pbase() == epptr())
            {
                std::string temp;
                temp += char(c);
                writeString(temp);
            }
            else
                sputc(c);
        }

        return 0;
    }
    int underflow()
    {
        std::string temp;
        readString(temp);
        strcpy(eback(), temp.c_str());
        setg(eback(), eback(), eback() + temp.size()-1);
    }

    int	sync()
    {
        if (pbase() != pptr())
        {
            int len = int(pptr() - pbase());
            std::string temp(pbase(), len);
            writeString(temp);
            setp(pbase(), epptr());
        }
        return 0;
    }
};

class ConsoleBuf : public BufferedStringBuf
{
public:
    ConsoleBuf() : BufferedStringBuf(LineSize) {}
    virtual void writeString(const std::string &str)
    {
       int txtlen;
       char *p;
       p = new char [str.size()+1];
       strcpy (p, str.c_str());
       p[str.size()-1] = 0;

       txtlen =(int)SendMessage(hEdit,WM_GETTEXTLENGTH,0,0);
       SendMessage(hEdit, EM_SETSEL, txtlen, txtlen);
       SendMessage(hEdit, EM_REPLACESEL, 0, (long)"COUT << ");
       SendMessage(hEdit, EM_REPLACESEL, 0, (long)p);
       SendMessage(hEdit, EM_REPLACESEL, 0, (long)"\r\n");
       delete[] p;
    };
    virtual void readString(std::string &str)
    {
       str = "Console_CIN_Not_Implemented\r\n ";      
    }
};

class InputBuf : public BufferedStringBuf
{
public:
    InputBuf() : BufferedStringBuf(LineSize) {}
    virtual void writeString(const std::string &str)
    {
        if ( str.size() > 1 ) // message box doesnt care about single characters
            MessageBox(NULL, str.c_str(), "Sortie sur CIN ??", MB_OK|MB_ICONERROR);
    }
;
    virtual void readString(std::string &str)
    {
       int txtlen;
       DialogBox(hinst, "CINDLG" , hMainWnd, (DLGPROC)CinDlgProc);
       str = strncat(stCin, "\r\n", 255);      
       txtlen =(int)SendMessage(hEdit,WM_GETTEXTLENGTH,0,0);
       SendMessage(hEdit, EM_SETSEL, txtlen, txtlen);
       SendMessage(hEdit, EM_REPLACESEL, 0, (long)" CIN >> ");
       SendMessage(hEdit, EM_REPLACESEL, 0, (long)stCin);
    }
};


class ErrorBoxBuf : public BufferedStringBuf
{
public:
    ErrorBoxBuf() : BufferedStringBuf(LineSize) {}
    virtual void writeString(const std::string &str)
    {
        if ( str.size() > 1 ) // message box doesnt care about single characters
            MessageBox(NULL, str.c_str(), "Error", MB_OK|MB_ICONERROR);
    }
;
    virtual void readString(std::string &str)
    {
       str = "Console_CERR_Not_Implemented\r\n ";      
    }
};

class LogBoxBuf : public BufferedStringBuf
{
public:
    LogBoxBuf() : BufferedStringBuf(LineSize) {}
    virtual void writeString(const std::string &str)
    {
        if ( str.size() > 1 ) // message box doesnt care about single characters
            MessageBox(NULL, str.c_str(), "CLOG", MB_OK);
    }
;
    virtual void readString(std::string &str)
    {
       str = "Console_CLOG_Not_Implemented\r\n ";      
    }
};


ConsoleBuf BufCOUT;
InputBuf BufCIN;
ErrorBoxBuf BufCERR;
LogBoxBuf BufCLOG;

//-----------------------------------------------------------------------------


int WINAPI WinMain (HINSTANCE hThisInstance,
                    HINSTANCE hPrevInstance,
                    LPSTR lpszArgument,
                    int nFunsterStil)

{
    HWND hwnd;               /* This is the handle for our window */
    MSG messages;            /* Here messages to the application are saved */
    WNDCLASSEX wincl;        /* Data structure for the windowclass */

    hinst = hThisInstance;

    /* The Window structure */
    wincl.hInstance = hThisInstance;
    wincl.lpszClassName = szClassName;
    wincl.lpfnWndProc = WindowProcedure;      /* This function is called by windows */
    wincl.style = CS_DBLCLKS;                 /* Catch double-clicks */
    wincl.cbSize = sizeof (WNDCLASSEX);

    /* Use default icon and mouse-pointer */
    wincl.hIcon = LoadIcon (NULL, IDI_APPLICATION);
    wincl.hIconSm = LoadIcon (NULL, IDI_APPLICATION);
    wincl.hCursor = LoadCursor (NULL, IDC_ARROW);
    wincl.lpszMenuName = NULL;                 /* No menu */
    wincl.cbClsExtra = 0;                      /* No extra bytes after the window class */
    wincl.cbWndExtra = 0;                      /* structure or the window instance */
    /* Use Windows's default color as the background of the window */
    wincl.hbrBackground = (HBRUSH) COLOR_BACKGROUND;

    /* Register the window class, and if it fails quit the program */
    if (!RegisterClassEx (&wincl))
        return 0;

    /* The class is registered, let's create the program*/
    hwnd = CreateWindowEx (
           0,                   /* Extended possibilites for variation */
           szClassName,         /* Classname */
           szCaption,           /* Title Text */
           WS_OVERLAPPEDWINDOW, /* default window */
           CW_USEDEFAULT,       /* Windows decides the position */
           CW_USEDEFAULT,       /* where the window ends up on the screen */
           544,                 /* The programs width */
           375,                 /* and height in pixels */
           HWND_DESKTOP,        /* The window is a child-window to desktop */
           NULL,                /* No menu */
           hThisInstance,       /* Program Instance handler */
           NULL                 /* No Window Creation data */
           );

    hMainWnd = hwnd;
    /* Make the window visible on the screen */
    ShowWindow (hwnd, nFunsterStil);

    /* Run the message loop. It will run until GetMessage() returns 0 */
    while (GetMessage (&messages, NULL, 0, 0))
    {
        /* Translate virtual-key messages into character messages */
        TranslateMessage(&messages);
        /* Send message to WindowProcedure */
        DispatchMessage(&messages);
    }

    /* The program return-value is 0 - The value that PostQuitMessage() gave */
    return messages.wParam;
}

/******************************************************************************/

BOOL APIENTRY CinDlgProc(HWND hDlg,UINT uMsg,WPARAM wParam,LPARAM lParam)
{
//    static HWND hParent;
    switch (uMsg)
    {
      case WM_INITDIALOG:
         return TRUE;

      case WM_COMMAND:
         if (LOWORD(wParam) == IDOK )
                {
                   GetDlgItemText(hDlg, IDE_EDIT1, stCin, 256);
                   EndDialog(hDlg,0);
                   return TRUE;
                }
      default:
         return FALSE;
    }
}

/******************************************************************************/


BOOL AddMainMenu (HWND hwndOwner)
{
  MainMenu=CreateMenu();
  FileMenu=CreateMenu();
  InsertMenu(MainMenu,ID_File,MF_POPUP,(UINT)FileMenu,"Fichier");
  AppendMenu(FileMenu,MF_STRING,ID_Execute,"&Executer");
  AppendMenu(FileMenu,MF_SEPARATOR,0,"");
  AppendMenu(FileMenu,MF_STRING,ID_Exit,"&Quiter");
  // activate menu
  return (SetMenu(hwndOwner, MainMenu));
}

BOOL AddConsole(HWND hwndOwner)
{
  HFONT hfont;   
  hEdit =CreateWindow("edit", "",
         WS_CHILD | WS_VISIBLE | ES_MULTILINE | ES_WANTRETURN | WS_VSCROLL,
         0, 0, 0, 0, hwndOwner, NULL, hinst, NULL);
  hfont = (HFONT)GetStockObject(ANSI_FIXED_FONT);                   
  SendMessage(hEdit, WM_SETFONT, (WPARAM) hfont, 0); 
  // replace the buffer
  std::cout.rdbuf(&BufCOUT);
  std::cin.rdbuf(&BufCIN);
  std::cerr.rdbuf(&BufCERR);
  std::clog.rdbuf(&BufCLOG);
return true;
}     

/*  This function is called by the Windows function DispatchMessage()  */

LRESULT CALLBACK WindowProcedure (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch (message)                  /* handle the messages */
    {
        case WM_CREATE: 
            AddMainMenu(hwnd);
            AddConsole(hwnd);
            break;
        case WM_COMMAND:
          switch LOWORD(wParam) {
            case ID_Execute:
              cout << "Execution du programme..." << endl;
              execute();
              break;
            case ID_Exit:
              PostQuitMessage(0);
              break;
          } //  
          break;     
        case WM_SIZE:
            MoveWindow(hEdit, 0, 0, LOWORD(lParam), HIWORD(lParam), TRUE);
            return 0;
        case WM_DESTROY:
            PostQuitMessage (0);       /* send a WM_QUIT to the message queue */
            break;
        default:                      /* for messages that we don't deal with */
            return DefWindowProc (hwnd, message, wParam, lParam);
    }

    return 0;
}
