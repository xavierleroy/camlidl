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

/* $Id: cfactory.cpp,v 1.9 2004-07-08 09:48:33 xleroy Exp $ */

/* The class factory and DLL support */

#include <string.h>
extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include "camlidlruntime.h"
}
#include "comstuff.h"
#include "registry.h"

#ifdef __CYGWIN32__
#include <sys/param.h>
#define _MAX_PATH MAXPATHLEN
#endif

/* Count of server locks */
static long camlidl_num_server_locks = 0;

/* The class factory */

class camlidl_factory : public IClassFactory
{
private:
  struct camlidl_comp * comp;
  long refcount;

public:

  virtual HRESULT __stdcall QueryInterface(const IID& iid, void** ppv)
  {
    if ((iid == IID_IUnknown) || (iid == IID_IClassFactory)) {
      *ppv = (IClassFactory*)(this);
      AddRef();
      return S_OK;
    } else {
      *ppv = NULL ;
      return E_NOINTERFACE ;
    }
  }
  virtual ULONG   __stdcall AddRef()
  {
    return InterlockedIncrement(&refcount);
  }
  virtual ULONG   __stdcall Release()
  {
    ULONG res = InterlockedDecrement(&refcount);
    if (res == 0) delete this;
    return res;
  }
  virtual HRESULT __stdcall CreateInstance(IUnknown* pUnknownOuter,
                                           const IID& iid,
                                           void** object) 
  {
    struct camlidl_ctx_struct ctx = { CAMLIDL_ADDREF, NULL };
    // Aggregation is not supported yet
    if (pUnknownOuter != NULL) return CLASS_E_NOAGGREGATION;
    // Create the component
    value vcomp =
      callback(Field(this->comp->compdata, COMPDATA_CREATE), Val_unit);
    IUnknown * comp = (IUnknown *) camlidl_unpack_interface(vcomp, &ctx);
    // Get the requested interface
    HRESULT res = comp->QueryInterface(iid, object);
    // Release the initial pointer to the component
    // (if QueryInterface failed, it will destroy itself)
    comp->Release();
    // Return result of QueryInterface
    return res;
  }
  virtual HRESULT __stdcall LockServer(BOOL bLock)
  {
    if (bLock)
      InterlockedIncrement(&camlidl_num_server_locks);
    else
      InterlockedDecrement(&camlidl_num_server_locks);
    return S_OK ;
  }

  // Constructor
  camlidl_factory(struct camlidl_comp * comp_init)
  {
    comp = comp_init;
    refcount = 1;
  }
};

// The class factory server

STDAPI DllGetClassObject(const CLSID & clsid, const IID & iid, void ** object)
{
  struct camlidl_comp * c;
  for (c = camlidl_registered_components; c != NULL; c = c->next) {
    if (clsid == GUID_val(Field(c->compdata, COMPDATA_CLSID))) {
      // Create class factory
      camlidl_factory * f = new camlidl_factory(c);
      if (f == NULL) return E_OUTOFMEMORY;
      // Get requested interface
      HRESULT res = f->QueryInterface(iid, object);
      // Release the class factory;
      // if QueryInterface failed, it will free itself
      f->Release();
      // Return result of QueryInterface
      return res;
    }
  }
  *object = NULL;
  return CLASS_E_CLASSNOTAVAILABLE;
}

/* Server registration */

STDAPI DllRegisterServer()
{
  struct camlidl_comp * c;
  for (c = camlidl_registered_components; c != NULL; c = c->next) {
    HRESULT retcode =
      RegisterServer(
        camlidl_module_handle,
        GUID_val(Field(c->compdata, COMPDATA_CLSID)),
        String_val(Field(c->compdata, COMPDATA_FRIENDLY_NAME)),
        String_val(Field(c->compdata, COMPDATA_VER_IND_PROG_ID)),
        String_val(Field(c->compdata, COMPDATA_PROG_ID)));
    if (FAILED(retcode)) return retcode;
  }
  return S_OK;
}

/* Server unregistration */

STDAPI DllUnregisterServer()
{
  struct camlidl_comp * c;

  for (c = camlidl_registered_components; c != NULL; c = c->next) {
    HRESULT retcode =
      UnregisterServer(
        GUID_val(Field(c->compdata, COMPDATA_CLSID)),
        String_val(Field(c->compdata, COMPDATA_VER_IND_PROG_ID)),
        String_val(Field(c->compdata, COMPDATA_PROG_ID)));
    if (FAILED(retcode)) return retcode;
  }
  return S_OK;
}

/* Can DLL unload now? */

STDAPI DllCanUnloadNow()
{
  if (camlidl_num_components == 0 && camlidl_num_server_locks == 0)
    return S_OK;
  else
    return S_FALSE;
}

#if 0
#include <fcntl.h>
#include <sys/stat.h>
#include <io.h>
#include <stdio.h>
#endif

/* DLL entry point */

BOOL APIENTRY DllMain(HANDLE module, DWORD reason, void *reserved)
{
  char * argv[2];
  char dll_path[_MAX_PATH];

  switch(reason) {
  case DLL_PROCESS_ATTACH:
    GetModuleFileName( (HMODULE) module, dll_path, _MAX_PATH );
    argv[0] = dll_path;
    argv[1] = NULL;
    camlidl_module_handle = (HMODULE) module;
#if 0
    int fd = open("/tmp/camllog", O_RDWR|O_TRUNC|O_CREAT, _S_IWRITE|_S_IREAD);
    dup2(fd, 1);
    dup2(fd, 2);
    close(fd);
#endif
    caml_startup(argv);
    break;
  /* TODO: free all memory when DLL detached */
  }
  return TRUE;
}
