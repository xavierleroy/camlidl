//
// Cmpnt.cpp
//
#include <iostream.h>
#include <objbase.h>

#include "Iface.h"      // Interface declarations
#include "Registry.h"   // Registry helper functions

// Trace function
void trace(const char* msg) { cout << msg << endl ;}

///////////////////////////////////////////////////////////
//
// Global variables
//
static HMODULE g_hModule = NULL ;   // DLL module handle
static long g_cComponents = 0 ;     // Count of active components
static long g_cServerLocks = 0 ;    // Count of locks

// Friendly name of component
const char g_szFriendlyName[] = "Inside COM, Chapter 7 Example" ;

// Version-independent ProgID
const char g_szVerIndProgID[] = "InsideCOM.Chap07" ;

// ProgID
const char g_szProgID[] = "InsideCOM.Chap07.1" ;


///////////////////////////////////////////////////////////
//
// Component 
//
class CA : public IX,
           public IY,
           public IZ
{
public:
	// IUnknown
	virtual HRESULT __stdcall QueryInterface(const IID& iid, void** ppv) ;
	virtual ULONG __stdcall AddRef() ;
	virtual ULONG __stdcall Release() ;

	// Interface IX
	virtual HRESULT __stdcall Fx() { cout << "Fx" << endl ; return S_OK; }

	// Interface IY
	virtual HRESULT __stdcall Fy(int x)
        { cout << "Fy: " << x << endl ; return S_OK; } 

	// Interface IZ
	virtual HRESULT __stdcall Fz(int x, int * y)
        { cout << "Fz: " << x << endl ;
          *y = 2 * x + 1;
          return S_OK; } 

	// Constructor
	CA() ;

	// Destructor
	~CA() ;

private:
	// Reference count
	long m_cRef ;
} ;


//
// Constructor
//
CA::CA() : m_cRef(1)
{ 
	InterlockedIncrement(&g_cComponents) ; 
}

//
// Destructor
//
CA::~CA() 
{ 
	InterlockedDecrement(&g_cComponents) ; 
	trace("Component:\t\tDestroy self.") ;
}

//
// IUnknown implementation
//
HRESULT __stdcall CA::QueryInterface(const IID& iid, void** ppv)
{    
	if (iid == IID_IUnknown)
	{
		*ppv = static_cast<IX*>(this) ; 
	}
	else if (iid == IID_IX)
	{
		*ppv = static_cast<IX*>(this) ;
		trace("Component:\t\tReturn pointer to IX.") ; 
	}
	else if (iid == IID_IY)
	{
		*ppv = static_cast<IY*>(this) ; 
		trace("Component:\t\tReturn pointer to IY.") ; 
	}
	else if (iid == IID_IZ)
	{
		*ppv = static_cast<IZ*>(this) ; 
		trace("Component:\t\tReturn pointer to IZ.") ; 
	}
	else
	{
		*ppv = NULL ;
		return E_NOINTERFACE ;
	}
	reinterpret_cast<IUnknown*>(*ppv)->AddRef() ;
	return S_OK ;
}

ULONG __stdcall CA::AddRef()
{
	return InterlockedIncrement(&m_cRef) ;
}

ULONG __stdcall CA::Release() 
{
	if (InterlockedDecrement(&m_cRef) == 0)
	{
		delete this ;
		return 0 ;
	}
	return m_cRef ;
}


///////////////////////////////////////////////////////////
//
// Class factory
//
class CFactory : public IClassFactory
{
public:
	// IUnknown
	virtual HRESULT __stdcall QueryInterface(const IID& iid, void** ppv) ;         
	virtual ULONG   __stdcall AddRef() ;
	virtual ULONG   __stdcall Release() ;

	// Interface IClassFactory
	virtual HRESULT __stdcall CreateInstance(IUnknown* pUnknownOuter,
	                                         const IID& iid,
	                                         void** ppv) ;
	virtual HRESULT __stdcall LockServer(BOOL bLock) ; 

	// Constructor
	CFactory() : m_cRef(1) {}

	// Destructor
	~CFactory() { trace("Class factory:\t\tDestroy self.") ;}

private:
	long m_cRef ;
} ;

//
// Class factory IUnknown implementation
//
HRESULT __stdcall CFactory::QueryInterface(const IID& iid, void** ppv)
{    
	if ((iid == IID_IUnknown) || (iid == IID_IClassFactory))
	{
		*ppv = static_cast<IClassFactory*>(this) ; 
	}
	else
	{
		*ppv = NULL ;
		return E_NOINTERFACE ;
	}
	reinterpret_cast<IUnknown*>(*ppv)->AddRef() ;
	return S_OK ;
}

ULONG __stdcall CFactory::AddRef()
{
	return InterlockedIncrement(&m_cRef) ;
}

ULONG __stdcall CFactory::Release() 
{
	if (InterlockedDecrement(&m_cRef) == 0)
	{
		delete this ;
		return 0 ;
	}
	return m_cRef ;
}

//
// IClassFactory implementation
//
HRESULT __stdcall CFactory::CreateInstance(IUnknown* pUnknownOuter,
                                           const IID& iid,
                                           void** ppv) 
{
	trace("Class factory:\t\tCreate component.") ;

	// Cannot aggregate.
	if (pUnknownOuter != NULL)
	{
		return CLASS_E_NOAGGREGATION ;
	}

	// Create component.
	CA* pA = new CA ;
	if (pA == NULL)
	{
		return E_OUTOFMEMORY ;
	}

	// Get the requested interface.
	HRESULT hr = pA->QueryInterface(iid, ppv) ;

	// Release the IUnknown pointer.
	// (If QueryInterface failed, component will delete itself.)
	pA->Release() ;
	return hr ;
}

// LockServer
HRESULT __stdcall CFactory::LockServer(BOOL bLock) 
{
	if (bLock)
	{
		InterlockedIncrement(&g_cServerLocks) ; 
	}
	else
	{
		InterlockedDecrement(&g_cServerLocks) ;
	}
	return S_OK ;
}


///////////////////////////////////////////////////////////
//
// Exported functions
//

//
// Can DLL unload now?
//
STDAPI DllCanUnloadNow()
{
	if ((g_cComponents == 0) && (g_cServerLocks == 0))
	{
		return S_OK ;
	}
	else
	{
		return S_FALSE ;
	}
}

//
// Get class factory
//
STDAPI DllGetClassObject(const CLSID& clsid,
                         const IID& iid,
                         void** ppv)
{
	trace("DllGetClassObject:\tCreate class factory.") ;

	// Can we create this component?
	if (clsid != CLSID_Component1)
	{
		return CLASS_E_CLASSNOTAVAILABLE ;
	}

	// Create class factory.
	CFactory* pFactory = new CFactory ;  // Reference count set to 1
	                                     // in constructor
	if (pFactory == NULL)
	{
		return E_OUTOFMEMORY ;
	}

	// Get requested interface.
	HRESULT hr = pFactory->QueryInterface(iid, ppv) ;
	pFactory->Release() ;

	return hr ;
}

//
// Server registration
//
STDAPI DllRegisterServer()
{
	return RegisterServer(g_hModule, 
	                      CLSID_Component1,
	                      g_szFriendlyName,
	                      g_szVerIndProgID,
	                      g_szProgID) ;
}


//
// Server unregistration
//
STDAPI DllUnregisterServer()
{
	return UnregisterServer(CLSID_Component1,
	                        g_szVerIndProgID,
	                        g_szProgID) ;
}

///////////////////////////////////////////////////////////
//
// DLL module information
//
BOOL APIENTRY DllMain(HANDLE hModule,
                      DWORD dwReason,
                      void* lpReserved)
{
	if (dwReason == DLL_PROCESS_ATTACH)
	{
		g_hModule = (HMODULE) hModule ;
	}
	return TRUE ;
}
