/* this ALWAYS GENERATED file contains the definitions for the interfaces */


/* File created by MIDL compiler version 5.01.0164 */
/* at Mon Mar 15 14:37:47 1999
 */
/* Compiler settings for Server.idl:
    Os (OptLev=s), W1, Zp8, env=Win32, ms_ext, c_ext
    error checks: allocation ref bounds_check enum stub_data 
*/
//@@MIDL_FILE_HEADING(  )


/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 440
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif // __RPCNDR_H_VERSION__

#ifndef COM_NO_WINDOWS_H
#include "windows.h"
#include "ole2.h"
#endif /*COM_NO_WINDOWS_H*/

#ifndef __Iface_h__
#define __Iface_h__

#ifdef __cplusplus
extern "C"{
#endif 

/* Forward Declarations */ 

#ifndef __IX_FWD_DEFINED__
#define __IX_FWD_DEFINED__
typedef interface IX IX;
#endif 	/* __IX_FWD_DEFINED__ */


#ifndef __Component_FWD_DEFINED__
#define __Component_FWD_DEFINED__

#ifdef __cplusplus
typedef class Component Component;
#else
typedef struct Component Component;
#endif /* __cplusplus */

#endif 	/* __Component_FWD_DEFINED__ */


/* header files for imported files */
#include "oaidl.h"

void __RPC_FAR * __RPC_USER MIDL_user_allocate(size_t);
void __RPC_USER MIDL_user_free( void __RPC_FAR * ); 

#ifndef __IX_INTERFACE_DEFINED__
#define __IX_INTERFACE_DEFINED__

/* interface IX */
/* [oleautomation][dual][unique][helpstring][uuid][object] */ 


EXTERN_C const IID IID_IX;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("32BB8326-B41B-11CF-A6BB-0080C7B2D682")
    IX : public IDispatch
    {
    public:
        virtual HRESULT STDMETHODCALLTYPE Fx( void) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE FxStringIn( 
            /* [in] */ BSTR bstrIn) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE FxStringOut( 
            /* [retval][out] */ BSTR __RPC_FAR *pbstrOut) = 0;
        
        virtual HRESULT STDMETHODCALLTYPE FxFakeError( void) = 0;
        
    };
    
#else 	/* C style interface */

    typedef struct IXVtbl
    {
        BEGIN_INTERFACE
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *QueryInterface )( 
            IX __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [iid_is][out] */ void __RPC_FAR *__RPC_FAR *ppvObject);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *AddRef )( 
            IX __RPC_FAR * This);
        
        ULONG ( STDMETHODCALLTYPE __RPC_FAR *Release )( 
            IX __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfoCount )( 
            IX __RPC_FAR * This,
            /* [out] */ UINT __RPC_FAR *pctinfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetTypeInfo )( 
            IX __RPC_FAR * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *GetIDsOfNames )( 
            IX __RPC_FAR * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
            /* [in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
        
        /* [local] */ HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Invoke )( 
            IX __RPC_FAR * This,
            /* [in] */ DISPID dispIdMember,
            /* [in] */ REFIID riid,
            /* [in] */ LCID lcid,
            /* [in] */ WORD wFlags,
            /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
            /* [out] */ VARIANT __RPC_FAR *pVarResult,
            /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
            /* [out] */ UINT __RPC_FAR *puArgErr);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *Fx )( 
            IX __RPC_FAR * This);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *FxStringIn )( 
            IX __RPC_FAR * This,
            /* [in] */ BSTR bstrIn);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *FxStringOut )( 
            IX __RPC_FAR * This,
            /* [retval][out] */ BSTR __RPC_FAR *pbstrOut);
        
        HRESULT ( STDMETHODCALLTYPE __RPC_FAR *FxFakeError )( 
            IX __RPC_FAR * This);
        
        END_INTERFACE
    } IXVtbl;

    interface IX
    {
        CONST_VTBL struct IXVtbl __RPC_FAR *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IX_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl -> QueryInterface(This,riid,ppvObject)

#define IX_AddRef(This)	\
    (This)->lpVtbl -> AddRef(This)

#define IX_Release(This)	\
    (This)->lpVtbl -> Release(This)


#define IX_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo)

#define IX_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define IX_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IX_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)


#define IX_Fx(This)	\
    (This)->lpVtbl -> Fx(This)

#define IX_FxStringIn(This,bstrIn)	\
    (This)->lpVtbl -> FxStringIn(This,bstrIn)

#define IX_FxStringOut(This,pbstrOut)	\
    (This)->lpVtbl -> FxStringOut(This,pbstrOut)

#define IX_FxFakeError(This)	\
    (This)->lpVtbl -> FxFakeError(This)

#endif /* COBJMACROS */


#endif 	/* C style interface */



HRESULT STDMETHODCALLTYPE IX_Fx_Proxy( 
    IX __RPC_FAR * This);


void __RPC_STUB IX_Fx_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IX_FxStringIn_Proxy( 
    IX __RPC_FAR * This,
    /* [in] */ BSTR bstrIn);


void __RPC_STUB IX_FxStringIn_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IX_FxStringOut_Proxy( 
    IX __RPC_FAR * This,
    /* [retval][out] */ BSTR __RPC_FAR *pbstrOut);


void __RPC_STUB IX_FxStringOut_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);


HRESULT STDMETHODCALLTYPE IX_FxFakeError_Proxy( 
    IX __RPC_FAR * This);


void __RPC_STUB IX_FxFakeError_Stub(
    IRpcStubBuffer *This,
    IRpcChannelBuffer *_pRpcChannelBuffer,
    PRPC_MESSAGE _pRpcMessage,
    DWORD *_pdwStubPhase);



#endif 	/* __IX_INTERFACE_DEFINED__ */



#ifndef __ServerLib_LIBRARY_DEFINED__
#define __ServerLib_LIBRARY_DEFINED__

/* library ServerLib */
/* [helpstring][version][uuid] */ 


EXTERN_C const IID LIBID_ServerLib;

EXTERN_C const CLSID CLSID_Component;

#ifdef __cplusplus

class DECLSPEC_UUID("0C092C2C-882C-11CF-A6BB-0080C7B2D682")
Component;
#endif
#endif /* __ServerLib_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

unsigned long             __RPC_USER  BSTR_UserSize(     unsigned long __RPC_FAR *, unsigned long            , BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserMarshal(  unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
unsigned char __RPC_FAR * __RPC_USER  BSTR_UserUnmarshal(unsigned long __RPC_FAR *, unsigned char __RPC_FAR *, BSTR __RPC_FAR * ); 
void                      __RPC_USER  BSTR_UserFree(     unsigned long __RPC_FAR *, BSTR __RPC_FAR * ); 

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif
