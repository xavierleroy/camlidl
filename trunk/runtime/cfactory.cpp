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

/* Count of server locks */
static long camlidl_num_server_locks = 0;

/* Handle for module (for the DLL) */
static HMODULE camlidl_module_handle;

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
    // Aggregation is not supported yet
    if (pUnknownOuter != NULL) return CLASS_E_NOAGGREGATION;
    // Create the component
    value vcomp =
      callback(Field(this->comp->compdata, COMPDATA_CREATE), Val_unit);
    IUnknown * comp = (IUnknown *) camlidl_unpack_interface(vcomp, NULL);
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

#define CLSID_val(v) (*((CLSID *) Bp_val(v)))

STDAPI DllGetClassObject(const CLSID & clsid, const IID & iid, void ** object)
{
  struct camlidl_comp * c;
  for (c = camlidl_registered_components; c != NULL; c = c->next) {
    if (clsid == CLSID_val(Field(c->compdata, COMPDATA_CLSID))) {
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
        CLSID_val(Field(c->compdata, COMPDATA_CLSID)),
        String_val(Field(c->compdata, COMPDATA_FRIENDLY_NAME)),
        String_val(Field(c->compdata, COMPDATA_VER_IND_PROG_ID)),
        String_val(Field(c->compdata, COMPDATA_PROG_ID)));
    if (FAILED(retcode)) return retcode;
  }
  return S_OK;
}

/* Server unregistration */

STDAPI DllUnregisterserver()
{
  struct camlidl_comp * c;

  for (c = camlidl_registered_components; c != NULL; c = c->next) {
    HRESULT retcode =
      UnregisterServer(
        CLSID_val(Field(c->compdata, COMPDATA_CLSID)),
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

/* DLL entry point */

BOOL APIENTRY DllMain(HANDLE module, DWORD reason, void *reserved)
{
  char * argv[1];

  switch(reason) {
  case DLL_PROCESS_ATTACH:
    argv[0] = NULL;
    camlidl_module_handle = (HMODULE) module;
    caml_startup(argv);
    break;
  /* TODO: free all memory when DLL detached */
  }
  return TRUE;
}
