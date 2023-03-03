#ifndef __WRAPPER_H
#define __WRAPPER_H

/* MSC: disable warning about int-to-bool conversion (just affects performance) */
#pragma warning(disable: 4800)
/* MSC: disable warning about using different code page (just affects performance) */
#pragma warning(disable: 4819)

/* just to ensure that intptr_t exists */
#ifndef  _MSC_VER
#include <inttypes.h>
/* MSVC-6 defines _MSC_VER=1200 */
#elif _MSC_VER> 1200
#else
/* MSVC-6 does not define intptr_t */
typedef int intptr_t;
#endif


#include "wx/wx.h"
#if (wxVERSION_NUMBER >= 2600)
#include "wx/apptrait.h"
#endif
#if (wxVERSION_NUMBER < 2900)
#include "wx/tabctrl.h"
#endif
#include "wx/notebook.h"
#include "wx/spinctrl.h"
#include "wx/statline.h"
#include "wx/checklst.h"
#include "wx/treectrl.h"
#include "wx/splash.h"
#include "wx/grid.h"
#include "wx/calctrl.h"
#include "wx/dnd.h"
#include "wx/config.h"
#include "wx/imaglist.h"
#include "wx/listctrl.h"
#include "wx/splitter.h"
#include "wx/image.h"
#include "wx/clipbrd.h"
#include "wx/colordlg.h"
#include "wx/fontdlg.h"
#include "wx/sckipc.h"
#include "wx/html/helpctrl.h"
#include "wx/print.h"
#include "wx/sashwin.h"
#include "wx/laywin.h"
#include "wx/minifram.h"
#include "wx/mstream.h"
#include "wx/wizard.h"
#include "wx/socket.h"
#include "wx/artprov.h"
#include "wx/sound.h"
#include "wx/aui/auibar.h"
#include "wx/aui/dockart.h"
#include "wx/aui/auibook.h"
#include "wx/aui/framemanager.h"


typedef void (*ClosureFun)(const wxEvent* _evt,unsigned int _data );

typedef bool (*AppInitFunc)(void);


/* Miscellaneous helper functions */
/* Copies the contents of a wxString to a buffer and returns the length of the string */
int copyStrToBuf(void* dst, wxString& src);

/* A Closure is used to call foreign functions. They are closures
 because they don't just contain a function pointer but also some
 local data supplied at creation time. The closures are reference counted
 by 'Callbacks'. Each event handler uses callbacks to react to primitive
 events like EVT_LEFT_CLICK and EVT_MOTION. These callbacks invoke the
 corresponding closure. Due to reference counting, a single closure can
 handle a range of events.
*/
class wxClosure : public wxClientData
{
  protected:
    int         m_refcount;     /* callbacks reference count the closures */
    ClosureFun  m_fun;          /* the foreign function to call */
    unsigned int m_wxcl_id;
  public:
    wxClosure( ClosureFun fun, unsigned int wxcl_id);
    ~wxClosure();

    virtual void IncRef();
    virtual void DecRef();

    virtual void Invoke(const wxEvent* event );
};

class wxCallback: public wxObject
{
  private:
    wxClosure* m_closure;    /* the closure to invoke */
  public:
    wxCallback( wxClosure* closure );
    ~wxCallback();

    void Invoke(const wxEvent* event );
    wxClosure* GetClosure();
};

extern wxClosure* initClosure;    /* called on wxApp::OnInit */

class ELJApp: public wxApp
{
  public:
    bool OnInit (void);
    int  OnExit (void);
    void HandleEvent(const wxEvent& _evt);
    void InitZipFileSystem();
    void InitImageHandlers();
    void OnInitCmdLine(wxCmdLineParser& parser);
    bool OnCmdLineParsed(wxCmdLineParser& parser);
};

void delete_cptr(void* _obj);
int wxEvtHandler_Connect(void* _obj,int first,int last,int type,wxClosure* closure);
void wxEvtHandler_Bind(void* _obj,int type,int first,int last,wxClosure* closure);
wxClosure* wxEvtHandler_GetClosure(wxEvtHandler* evtHandler,int id,int type);
wxClosure* wxClosure_Create(ClosureFun fun,unsigned int data);
void* wxEvtHandler_GetClientClosure(void* _obj);
void wxObject_SetClientClosure(void* _obj,wxClosure* closure);
int ELJApp_GetIdleInterval();
void ELJApp_SetIdleInterval(int interval);



DECLARE_APP(ELJApp);

#endif /* #ifndef __WRAPPER_H */
