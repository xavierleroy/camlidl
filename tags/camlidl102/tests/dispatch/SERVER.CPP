#include "CFactory.h"
#include "Iface.h"
#include "Cmpnt.h"

///////////////////////////////////////////////////////////
//
// Server.cpp
//
// This file contains the component server code.
// The FactoryDataArray contains the components that 
// can be served.
//

// Each component derived from CUnknown defines a static function
// for creating the component with the following prototype. 
// HRESULT CreateInstance(IUnknown* pUnknownOuter, 
//                        CUnknown** ppNewComponent) ;
// This function is used to create the component.
//

//
// The following array contains the data used by CFactory
// to create components. Each element in the array contains
// the CLSID, the pointer to the creation function, and the name
// of the component to place in the Registry.
//
CFactoryData g_FactoryDataArray[] =
{
	{&CLSID_Component, CA::CreateInstance, 
		"Inside COM, Chapter 11 Example", // Friendly Name
		"InsideCOM.Chap11",        // ProgID
		"InsideCOM.Chap11.1",      // Version-independent ProgID
		&LIBID_ServerLib,          // Type Library ID
		NULL, 0}
} ;
int g_cFactoryDataEntries
	= sizeof(g_FactoryDataArray) / sizeof(CFactoryData) ;
