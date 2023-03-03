#include "wrapper.h"
#include "clcxx/clcxx.hpp"
#include <wx/window.h>
#include "frame.h"

extern int APPTerminating;


#ifdef _WIN32

#if (defined(__WXDEBUG__) && defined(_MSC_VER))
 #include <crtdbg.h>
#endif

#include <windows.h>
#include <locale.h>

#if wxCHECK_VERSION(2,5,0)
 #define wxHANDLE  HINSTANCE
 extern int WXDLLIMPEXP_BASE wxEntry(wxHANDLE hInstance, wxHANDLE hPrevInstance, char *pCmdLine, int nCmdShow);
#else
 #define wxHANDLE  WXHINSTANCE
#endif


void ELJApp_InitializeC (wxClosure* closure, int _argc, char** _argv)
{
  wxHANDLE wxhInstance = GetModuleHandle(NULL);

  /*
   * Set the locale to "C",
   * to prevent a wxWidgets assert failure in intl.cpp
   */
  setlocale(LC_ALL, "C");

/* check memory leaks with visual C++ */
#if (defined(__WXDEBUG__) && defined(_MSC_VER))
  _CrtMemState memStart,memEnd,memDif;
  _CrtMemCheckpoint( &memStart );
  _CrtSetReportMode( _CRT_WARN, _CRTDBG_MODE_FILE );
  _CrtSetReportFile( _CRT_WARN, _CRTDBG_FILE_STDOUT );
  _CrtSetReportMode( _CRT_ERROR, _CRTDBG_MODE_FILE );
  _CrtSetReportFile( _CRT_ERROR, _CRTDBG_FILE_STDOUT );
  _CrtSetReportMode( _CRT_ASSERT, _CRTDBG_MODE_FILE );
  _CrtSetReportFile( _CRT_ASSERT, _CRTDBG_FILE_STDOUT );
#endif

  initClosure = closure;
  APPTerminating = 0;
  
  /* Pretty lame way to detect if we are running in GHCI */
  /* TODO: detect when running program with runhaskell as well */
  if(_argc > 0 && !wcscmp(((wchar_t**)_argv)[0], L"<interactive>")) {
    /* we are in GHCI */
    wxEntry(_argc, _argv);
  }
  else {
    wxEntry(wxhInstance, NULL, (_argc > 0 ? _argv[0] : NULL), SW_SHOWNORMAL);
  }
  
  APPTerminating = 1;

  /* wxPendingEvents is deleted but not set to NULL -> disaster when restarted from an interpreter */
#if !defined(WXMAKINGDLL) && !defined(WXUSINGDLL)
  wxPendingEvents = NULL;
#endif

#if defined(wxUSE_ODBC) && (wxUSE_ODBC != 0)
  wxDbCloseConnections();
#endif

/* check memory leaks with visual C++ */
#if (defined(__WXDEBUG__) && defined(_MSC_VER))
  _CrtMemCheckpoint( &memEnd );
  if (_CrtMemDifference( &memDif, &memStart, &memEnd)
     && (memDif.lCounts[_NORMAL_BLOCK]>=-2 && memDif.lCounts[_NORMAL_BLOCK] <= 0))
  {
    _RPT0(_CRT_WARN,"\n** memory leak detected (**\n" );
    _CrtMemDumpStatistics(&memDif);
    /* _CrtMemDumpAllObjectsSince(&memStart);  */
    _RPT0(_CRT_WARN,"** memory leak report done **\n\n" );

  }
#endif
}

#else  /* not WIN32 */

#if !wxCHECK_VERSION(2,5,0)
#ifdef __WXMAC__ /* declare wxEntry explicitly as wxMAC seems to leave it out? */
void wxEntry( int argc, char** argv, bool enterLoop = true );
#endif
#ifdef __WXGTK__ /* declare explicitly or we get link errors? */
int wxEntry( int argc, char** argv );
#endif
#endif

void ELJApp_InitializeC (wxClosure* closure, int _argc, char** _argv)
{
  //char* args[] = { "wxc", NULL };
  char **v = new char*[2];
  v[0] = strdup("wxc");
  v[1] = NULL;
  initClosure = closure;
  if (_argv == NULL) {
    /* note: wxGTK crashes when argv == NULL */
    _argv = v;//args;
    _argc = 1;
  }
  APPTerminating = 0;
  wxEntry(_argc,_argv);
  APPTerminating = 1;
  /* wxPendingEvents is deleted but not set to NULL -> disaster when restarted from an interpreter */
#if !defined(WXMAKINGDLL) && !defined(WXUSINGDLL)
  wxPendingEvents = NULL;
#endif
}

#endif


CLCXX_PACKAGE WXCL(clcxx::Package& pack) {
/*-----------------------------------------------------------------------------
     interface to event handling and closures.
-----------------------------------------------------------------------------*/
  pack.defun("%wx-evt-handler-connect", F_PTR(&wxEvtHandler_Connect));
  pack.defun("%wx-evt-handler-bind", F_PTR(&wxEvtHandler_Bind));
  pack.defun("%wx-evt-handler-get-closure", F_PTR(&wxEvtHandler_GetClosure));
  pack.defun("%wx-closure-create", F_PTR(&wxClosure_Create));
  pack.defun("%wx-evt-handler-Get-client-closure", F_PTR(&wxEvtHandler_GetClientClosure));
  pack.defun("%wx-object-set-client-closure", F_PTR(&wxObject_SetClientClosure));
/*-----------------------------------------------------------------------------
     interface to the idle timer
-----------------------------------------------------------------------------*/    
  pack.defun("app-get-idle-interval", F_PTR(&ELJApp_GetIdleInterval));
  pack.defun("app-set-idle-interval", F_PTR(&ELJApp_SetIdleInterval));
  
  pack.defun("%app-initialize-c", F_PTR(&ELJApp_InitializeC));
  pack.defun("%app-initialize", F_PTR(&ELJApp_initialize));  
/*-----------------------------------------------------------------------------
     wxFrame Class
-----------------------------------------------------------------------------*/      
  pack.defconstant("+default-frame-style+", wxDEFAULT_FRAME_STYLE);
  pack.defconstant("+iconize+", wxICONIZE);
  pack.defconstant("+caption+", wxCAPTION);
  pack.defconstant("+minimize+", wxMINIMIZE);
  pack.defconstant("+minimize-box+", wxMINIMIZE_BOX);
  pack.defconstant("+maximize+", wxMAXIMIZE);
  pack.defconstant("+maximize-box+", wxMAXIMIZE_BOX);
  pack.defconstant("+close-box+", wxCLOSE_BOX);
  pack.defconstant("+stay-on-top+", wxSTAY_ON_TOP);
  pack.defconstant("+system-menu+", wxSYSTEM_MENU);
  pack.defconstant("+resize-border+", wxRESIZE_BORDER);
  pack.defconstant("+frame-tool-window+", wxFRAME_TOOL_WINDOW);
  pack.defconstant("+frame-no-taskbar+", wxFRAME_NO_TASKBAR);
  pack.defconstant("+frame-float-on-parent+", wxFRAME_FLOAT_ON_PARENT);
  pack.defconstant("+frame-shaped+", wxFRAME_SHAPED);
      
  pack.defclass<eljframe, false>("frame")
      .defmethod("set-title", F_PTR(&eljframe::SetTitle))
      .defmethod("get-title", F_PTR(&eljframe::GetTitle))
      .defmethod("show", F_PTR(&wxFrame::Show))   
      .constructor<wxWindow*,int,char*,int,int,int,int,int>()
      .constructor<>();  
  
}  
