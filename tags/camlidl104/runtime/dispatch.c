/***********************************************************************/
/*                                                                     */
/*                              CamlIDL                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id: dispatch.c,v 1.2 2000-08-19 11:05:00 xleroy Exp $ */

/* Support for dispatch interfaces */

#include <string.h>
#include <stdio.h>
#include "camlidlruntime.h"
#include "comstuff.h"

/* Handle for module (for the DLL) */

HMODULE camlidl_module_handle = NULL;

/* Retrieves the number of type information interfaces that
   the object provides (either 0 or 1). */

HRESULT STDMETHODCALLTYPE
camlidl_GetTypeInfoCount(struct camlidl_intf * self, UINT * count_type_info)
{
  *count_type_info = 1;
  return S_OK;
}

/* Retrieves the type information for the object */

HRESULT STDMETHODCALLTYPE
camlidl_GetTypeInfo(struct camlidl_intf * self, UINT iTypeInfo,
                    LCID localization, ITypeInfo ** res)
{
  HRESULT hr;
  int i;
  ITypeInfo * tinfo;

  if (iTypeInfo != 0) {
    *res = NULL;
    return DISP_E_BADINDEX;
  }
  tinfo = (ITypeInfo *) self->typeinfo;
  /* Increase refcount of type info object */
  tinfo->lpVtbl->AddRef(tinfo);
  /* Return tinfo */
  *res = tinfo;
  return S_OK;
}

/* Find the dispatch IDs of the given method names */

HRESULT STDMETHODCALLTYPE
camlidl_GetIDsOfNames(struct camlidl_intf * self, REFIID iid,
                      OLECHAR** arrayNames, UINT countNames,
                      LCID localization, DISPID * arrayDispIDs)
{
  ITypeInfo * tinfo;

  if (! IsEqualIID(iid, &IID_NULL)) return DISP_E_UNKNOWNINTERFACE;
  tinfo = (ITypeInfo *) self->typeinfo;
  return tinfo->lpVtbl->GetIDsOfNames(tinfo, arrayNames,
                                      countNames, arrayDispIDs);
}  

/* Invoke a method by dynamic dispatch */

HRESULT STDMETHODCALLTYPE
camlidl_Invoke(struct camlidl_intf * self, DISPID dispidMember, REFIID iid,
               LCID localization, WORD wFlags, DISPPARAMS * dispParams,
               VARIANT * varResult, EXCEPINFO * excepInfo, UINT * argErr)
{
  ITypeInfo * tinfo;

  if (! IsEqualIID(iid, &IID_NULL)) return DISP_E_UNKNOWNINTERFACE;
  tinfo = (ITypeInfo *) self->typeinfo;
  SetErrorInfo(0, NULL);
  return tinfo->lpVtbl->Invoke(tinfo, (IDispatch *) self,
                               dispidMember, wFlags, dispParams,
                               varResult, excepInfo, argErr);
}

/* Load the type info library for the object and store it in the
   typeinfo field of the object */

static int camlidl_num_type_libraries = 0;
static void camlidl_read_num_type_libraries(void);

IUnknown * camlidl_find_typeinfo(IID * iid)
{
  ITypeLib * tlib;
  ITypeInfo * tinfo;
  char module_path[_MAX_PATH];
  char resname[_MAX_PATH + 4];
  wchar_t wresname[_MAX_PATH + 4];
  int i;
  HRESULT hr;

  /* Determine number of type libraries available (if not already done) */
  if (camlidl_num_type_libraries == 0) camlidl_read_num_type_libraries();
  /* Get the full name of the executable */
  GetModuleFileName(camlidl_module_handle, module_path, _MAX_PATH);
  /* Load the type libraries and query them */
  for (i = 1; i <= camlidl_num_type_libraries; i++) {
    /* Build the wide string <module_path>\<i> */
    sprintf(resname, "%s\\%d", module_path, i);
    mbstowcs(wresname, resname, _MAX_PATH + 4);
    /* Load the type library */
    hr = LoadTypeLib(wresname, &tlib);
    if (FAILED(hr))
      camlidl_error(hr, "Com.create_dispatch", "Cannot load type library");
    /* Query the type library for the type info for the object */
    hr = tlib->lpVtbl->GetTypeInfoOfGuid(tlib, iid, &tinfo);
    /* Release the library */
    tlib->lpVtbl->Release(tlib);
    if (SUCCEEDED(hr)) return (IUnknown *) tinfo;
  }
  /* Not found: raise an exception */
  camlidl_error(TYPE_E_ELEMENTNOTFOUND, "Com.make_",
                "Cannot find type library for interface");
  return NULL; /* not reached */
}

static void camlidl_read_num_type_libraries(void)
{
  HRSRC hFound;
  HGLOBAL hRes;
  void * lpBuff;

  hFound = FindResource(camlidl_module_handle, (char *)1, "num_typelibs");
  if (hFound == NULL)
    camlidl_error(0, "Com.make_", "Cannot find resource num_typelibs");
  hRes = LoadResource(camlidl_module_handle, hFound);
  if (hRes == NULL)
    camlidl_error(0, "Com.make_", "Cannot load resource num_typelibs");
  lpBuff = LockResource(hRes);
  camlidl_num_type_libraries = *((WORD *) lpBuff);
  UnlockResource(hRes);
  FreeResource(hRes);
}
