//
// Client.cpp - client implementation
//
#include <iostream.h>
#include <objbase.h>

#include "Iface.h"

void trace(const char* msg) { cout << "Client: \t\t" << msg << endl ;}

// test one component

static void testcomp(const CLSID & clsid)
{
	trace("Call CoCreateInstance to create") ;
	trace("  component and get interface IX.") ;
	IX* pIX = NULL ; 
	HRESULT hr = ::CoCreateInstance(clsid,
	                                NULL, 
	                                CLSCTX_INPROC_SERVER,
	                                IID_IX, 
	                                (void**)&pIX) ;
	if (SUCCEEDED(hr))
	{
		trace("Succeeded getting IX.") ;
		pIX->Fx() ;          // Use interface IX.

		trace("Ask for interface IY.") ;
		IY* pIY = NULL ;
		hr = pIX->QueryInterface(IID_IY, (void**)&pIY) ;
		if (SUCCEEDED(hr))
		{
			trace("Succeeded getting IY.") ;
			pIY->Fy(12) ;       // Use interface IY.
			pIY->Release() ;
			trace("Release IY interface.") ;
		}
		else
		{
			trace("Could not get interface IY.") ;
		}

		trace("Ask for interface IZ.") ;

		IZ* pIZ = NULL ;
		hr = pIX->QueryInterface(IID_IZ, (void**)&pIZ) ;
		if (SUCCEEDED(hr))
		{
                        int res;
			trace("Succeeded in getting interface IZ.") ;
			pIZ->Fz(18, &res) ;
                        cout << "Client: \t\tFz(18) returns " << res << endl;
			pIZ->Release() ;
			trace("Release IZ interface.") ;
		}
		else
		{
			trace("Could not get interface IZ.") ;
		}

		trace("Release IX interface.") ;
		pIX->Release() ;
	}
	else
	{
		cout << "Client: \t\tCould not create component. hr = "
		     << hex << hr << endl ;    
	}
}


//
// main function
//
int main()
{
	// Initialize COM Library
	CoInitialize(NULL) ;

        // Testing the two components
        trace("Testing Component 1...");
        testcomp(CLSID_Component1);
        trace("Testing Component 2...");
        testcomp(CLSID_Component2);

	// Uninitialize COM Library
	CoUninitialize() ;

	return 0 ;
}
