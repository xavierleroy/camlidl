//
// Client.cpp - Client implementation
//   This client connects to the IX dual interface
//   through the vtbl.
//
#include <objbase.h>
#include <iostream.h>

#include "Util.h"
#include "Iface.h"

static inline void trace(char* msg)
	{ Util::Trace("Client", msg, S_OK) ;} 
static inline void trace(char* msg, HRESULT hr)
	{ Util::Trace("Client", msg, hr) ;}

void test(const CLSID & clsid)
{
	DWORD clsctx ;
	clsctx = CLSCTX_INPROC_SERVER ;		
	trace("Attempt to create in-proc component.") ;

	IX* pIX = NULL ; 
	HRESULT hr = CoCreateInstance(clsid,
	                              NULL,
	                              clsctx, 
	                              IID_IX,
	                              (void**)&pIX) ;
	if (SUCCEEDED(hr))
	{
		trace("Successfully created component.") ;
		trace("Use interface IX.") ;
		wchar_t* wszIn = L"This is the test." ;
		BSTR bstrIn ;
		bstrIn = ::SysAllocString(wszIn) ; 
		pIX->FxStringIn(bstrIn) ; 
		::SysFreeString(bstrIn) ;

		BSTR bstrOut ; //@dual
		pIX->FxStringOut(&bstrOut ) ;

		// Display returned string.
		ostrstream sout ;
		sout << "FxStringOut returned a string:  "
		     << bstrOut 
		     << ends;
		trace(sout.str()) ;
		::SysFreeString(bstrOut ) ;

		trace("Release IX.") ;
		pIX->Release() ;
	}
	else
	{
		trace("Could not create component.", hr);
	}

}

const CLSID CLSID_Caml_Component = 
  {0x6a3d0750, 0xdad9, 0x11d2, {0x8e, 0x2c, 0x00, 0x60, 0x97, 0x4f, 0xbf, 0x19} };

int main()
{
  // Initialize COM Library
  CoInitialize(NULL) ;
  test(CLSID_Component);
  test(CLSID_Caml_Component);
  // Uninitialize COM Library
  CoUninitialize() ;
  return 0;
}
