#include <wx/frame.h>


class eljframe : public wxFrame {
public:
  eljframe() : wxFrame() {};
  eljframe(wxWindow* prt,int id,char* txt,int lft,int top,int wdt,int hgt,int stl)
    : wxFrame (prt, id, wxString(txt), wxPoint(lft, top), wxSize(wdt, hgt), stl) {};
  void SetTitle (char* str) { wxFrame::SetTitle(wxString(str));};
  std::string GetTitle(void) {return std::string(wxFrame::GetTitle().mb_str());};
};   
  

