#include "wrapper.h"
#include "wx/tooltip.h"
#include "wx/dynlib.h"
#include "wx/fs_zip.h"
#include "wx/cmdline.h"


/* quantize is not supported on wxGTK 2.4.0 */
#if !defined(__WXGTK__) || (wxVERSION_NUMBER > 2400)
#  include "wx/quantize.h"
#endif

/*-----------------------------------------------------------------------------
    Miscellaneous helper functions
-----------------------------------------------------------------------------*/

int copyStrToBuf(void* dst, wxString& src) {
  if (dst) wxStrcpy ((wxChar*) dst, src.c_str());
  return src.Length();
}

/*-----------------------------------------------------------------------------
    The global idle timer
-----------------------------------------------------------------------------*/
class wxIdleTimer : public wxTimer
{
public:
  void Notify() {
    wxWakeUpIdle();   /* send idle events */
  }
};

wxIdleTimer* idleTimer = nullptr;

void initIdleTimer()
{
  idleTimer = new wxIdleTimer();
}

void doneIdleTimer()
{
  if (idleTimer) {
    idleTimer->Stop();
    delete idleTimer;
    idleTimer = nullptr;
  }
}



/*-----------------------------------------------------------------------------
    The main application
-----------------------------------------------------------------------------*/
wxClosure* initClosure = nullptr;
int APPTerminating = 0;

IMPLEMENT_APP_NO_MAIN(ELJApp);

bool ELJApp::OnInit (void)
{
  if (!wxApp::OnInit())
    return false;

  initIdleTimer();
  wxInitAllImageHandlers();
  if (initClosure) {
    delete initClosure; /* special: init is only called once with a nullptr event */
    initClosure=nullptr;
  }
  return true;
}

int ELJApp::OnExit( void )
{
  doneIdleTimer();
  return wxApp::OnExit();
}

void ELJApp::InitZipFileSystem()
{
        static int InitZIPFileSystem_done = 0;

        if (!InitZIPFileSystem_done)
        {
                InitZIPFileSystem_done = 1;
                wxFileSystem::AddHandler(new wxZipFSHandler());
        }
}

void ELJApp::InitImageHandlers()
{
  wxInitAllImageHandlers();
}


/* "getCallback" is a hack to retrieve the callback object for a certain event
   see also "wxEvtHandler_FindClosure"
*/
static wxCallback** getCallback = nullptr;

void ELJApp::HandleEvent(const wxEvent& _evt)
{
  wxCallback* callback = (wxCallback*)(_evt.GetEventUserData());
  if (getCallback) {
    *getCallback = callback;    /* retrieve the callback */
  }
  else if (callback) {
    callback->Invoke( &_evt );  /* normal: invoke the callback function */
  }
}

/* override to prevent parent wxApp failing to parse Haskell cmdline args */
void ELJApp::OnInitCmdLine(wxCmdLineParser& parser)
{
  parser.SetCmdLine("");
}

/* override to prevent parent wxApp from further processing of parsed cmdline */
bool ELJApp::OnCmdLineParsed(wxCmdLineParser& parser)
{
  return true;
}

/*-----------------------------------------------------------------------------
    Closures
-----------------------------------------------------------------------------*/
wxClosure::wxClosure( ClosureFun fun, unsigned int wxcl_id)
{
  m_refcount = 0;
  m_fun  = fun;
  m_wxcl_id = wxcl_id;
}

wxClosure::~wxClosure()
{
  /* call for the last time with a nullptr event. Give opportunity to clean up resources */
  if (m_fun) { m_fun(nullptr,m_wxcl_id); }
}

void wxClosure::IncRef()
{
  m_refcount++;
}

void wxClosure::DecRef()
{
  m_refcount--;
  if (m_refcount <= 0) {
    delete this;
  }
}

void wxClosure::Invoke(const wxEvent* event )
{
  if (event && m_fun) { m_fun(event,m_wxcl_id); }
}

/*-----------------------------------------------------------------------------
    callback: a reference counting wrapper for a closure
-----------------------------------------------------------------------------*/
wxCallback::wxCallback( wxClosure* closure )
{
  m_closure = closure;
  m_closure->IncRef();
}

wxCallback::~wxCallback()
{
  m_closure->DecRef();
}

void wxCallback::Invoke(const wxEvent* event )
{
  m_closure->Invoke(event);
}

wxClosure* wxCallback::GetClosure()
{
  return m_closure;
}



/*-----------------------------------------------------------------------------
    wrapper for objectRefData
-----------------------------------------------------------------------------*/
class wxcClosureRefData : public wxObjectRefData
{
  private:
    wxClosure*  m_closure;
  public:
    wxcClosureRefData( wxClosure* closure );
    ~wxcClosureRefData();

    wxClosure* GetClosure();
};

wxcClosureRefData::wxcClosureRefData( wxClosure* closure )
{
  m_closure = closure;
}

wxcClosureRefData::~wxcClosureRefData()
{
/* printf("delete wxc-ClosureRefData\n");  */
  if (m_closure) { delete m_closure; m_closure = nullptr; }
}

wxClosure* wxcClosureRefData::GetClosure()
{
  return m_closure;
}


/*-----------------------------------------------------------------------------
    C interface to event handling and closures.
-----------------------------------------------------------------------------*/

/* delete c pointers*/
void delete_cptr(void* _obj)
{
  if (_obj == nullptr)
      return;	  
  delete _obj;
  _obj = nullptr;
}  
	
/* event handling */
int wxEvtHandler_Connect(void* _obj,int first,int last,int type,wxClosure* closure)
{
  wxCallback* callback = new wxCallback(closure);
  ((wxEvtHandler*)_obj)->Connect(first, last, type, (wxObjectEventFunction)&ELJApp::HandleEvent, callback);
  return 0;
}

void wxEvtHandler_Bind(void* _obj,int type,int first,int last,wxClosure* closure)
{
  wxCallback* callback = new wxCallback(closure);
  ((wxEvtHandler*)_obj)->Bind(type,(wxObjectEventFunction)&ELJApp::HandleEvent,(wxEvtHandler*)_obj,first, last, callback);
}



wxClosure* wxEvtHandler_GetClosure(wxEvtHandler* evtHandler,int id,int type)
{
  wxCommandEvent  event(type,id);     //We can use any kind of event here
  wxCallback*     callback = nullptr;
  bool            found    = false;

  //set the global variable 'getCallback' so HandleEvent
  //knows we just want to know the closure. Unfortunately, this
  //seems the cleanest way to retrieve the callback in wxWidgets.
  getCallback = &callback;

#if wxCHECK_VERSION(3, 1, 0)
  #pragma GCC warning "wxEvtHandler_GetClosure must be studied carefully for wxWidgets >= 3.1.0, 'class wxEvtHandler' has no member named 'GetDynamicEventTable' anymore"
#else
  // Bugfix: see www.mail-archive.com/wxhaskell-devel@lists.sourceforge.net/msg00577.html
  // On entry, Dynamic event table may have no bound events
  // Bug reproduces only on Debug builds, and seems to be ignorable
  if (evtHandler->GetDynamicEventTable() != nullptr)
    found = evtHandler->SearchDynamicEventTable( event );
#endif
  getCallback = nullptr;

  if (found && callback)
    return callback->GetClosure();
  else
    return nullptr;
}

/* closures */
wxClosure* wxClosure_Create(ClosureFun fun,unsigned int data)
{
  return new wxClosure(fun,data);
}

/* client data */
void* wxEvtHandler_GetClientClosure(void* _obj)
{
  return (void*)((wxEvtHandler*)_obj)->GetClientObject();
}

void wxEvtHandler_SetClientClosure(void* _obj,wxClosure* closure)
{
  ((wxEvtHandler*)_obj)->SetClientObject(closure);
}

wxClosure* wxObject_GetClientClosure(wxObject* _obj)
{
  wxcClosureRefData* refData = (wxcClosureRefData*)_obj->GetRefData();
  if (refData)
    return refData->GetClosure();
  else
    return nullptr;
}

void wxObject_SetClientClosure(void* _obj,wxClosure* closure)
{
  wxcClosureRefData* refData;
  /* wxASSERT(_obj->GetRefData() == nullptr); */
  ((wxObject*)_obj)->UnRef();
  wxASSERT(((wxObject*)_obj)->GetRefData() == nullptr);
  refData = new wxcClosureRefData( closure );
  ((wxObject*)_obj)->SetRefData( refData ); //set new data -- ref count must be 1 as setRefData doesn't increase it.  
}

/*-----------------------------------------------------------------------------
    C interface to the idle timer
-----------------------------------------------------------------------------*/
int ELJApp_GetIdleInterval()
{
  if (!idleTimer) return 0;

  if (idleTimer->IsRunning())
    return idleTimer->GetInterval();
  else
    return 0;
}

void ELJApp_SetIdleInterval(int interval)
{
  if (idleTimer) {
    if (idleTimer->IsRunning()) {
      idleTimer->Stop();
    }
    if (interval >= 5) {
      idleTimer->Start( interval, false );
    }
  }
}


