//
// Cmpnt.cpp - Component
//

#include "Iface.h"
#include "CUnknown.h" 

///////////////////////////////////////////////////////////
//
// Component A
//
class CA : public CUnknown,
           public IX,
           public ISupportErrorInfo
{
public:	
	// Creation
	static HRESULT CreateInstance(IUnknown* pUnknownOuter,
	                              CUnknown** ppNewComponent ) ;

private:
	// Declare the delegating IUnknown.
	DECLARE_IUNKNOWN

	// IUnknown
	virtual HRESULT __stdcall NondelegatingQueryInterface(const IID& iid,
	                                                      void** ppv) ;

	// IDispatch
	virtual HRESULT __stdcall GetTypeInfoCount(UINT* pCountTypeInfo) ;

	virtual HRESULT __stdcall GetTypeInfo(
		UINT iTypeInfo,
		LCID,              // Localization is not supported.
		ITypeInfo** ppITypeInfo) ;
	
	virtual HRESULT __stdcall GetIDsOfNames(
		const IID& iid,
		OLECHAR** arrayNames,
		UINT countNames,
		LCID,              // Localization is not supported.
		DISPID* arrayDispIDs) ;

	virtual HRESULT __stdcall Invoke(   
		DISPID dispidMember,
		const IID& iid,
		LCID,              // Localization is not supported.
		WORD wFlags,
		DISPPARAMS* pDispParams,
		VARIANT* pvarResult,
		EXCEPINFO* pExcepInfo,
		UINT* pArgErr) ;

	// Interface IX
	virtual HRESULT  __stdcall Fx() ;
	virtual HRESULT __stdcall FxStringIn(BSTR bstrIn) ; 
	virtual HRESULT __stdcall FxStringOut(BSTR* pbstrOut) ;
	virtual HRESULT __stdcall FxFakeError() ;

	// ISupportErrorInfo
	virtual HRESULT __stdcall InterfaceSupportsErrorInfo(const IID& riid)
	{
		return (riid == IID_IX) ? S_OK : S_FALSE ;
	}

	// Initialization
 	virtual HRESULT Init() ;

	// Constructor
	CA(IUnknown* pUnknownOuter) ;

	// Destructor
	~CA() ;

	// Pointer to type information.
	ITypeInfo* m_pITypeInfo ;
} ;
