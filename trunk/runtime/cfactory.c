/* The class factory */

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include "camlidlruntime.h"
#include "comstuff.h"

/* The list of all registered components */

struct camlidl_comp {
  value compdata;
  struct camlidl_comp * next;
};

static struct camlidl_comp * registered_components = NULL;

/* Structure of the "compdata" Caml record */

#define COMPDATA_CREATE 0
#define COMPDATA_CLSID 1
#define COMPDATA_FRIENDLY_NAME 2
#define COMPDATA_VER_IND_PROG_ID 3
#define COMPDATA_PROG_ID 4

/* Register a Caml component factory */

value camlidl_register_factory(value compdata)
{
  struct camlidl_comp * c = stat_alloc(sizeof(struct camlidl_comp));
  c->compdata = compdata;
  register_global_root(&(c->compdata));
  c->next = registered_components;
  registered_components = c;
  return Val_unit;
}

#ifdef _WIN32

/* The class factory itself */

struct camlidl_factory {
  struct IClassFactoryVtbl * lpVtbl;
  int refcount;
  struct camlidl_comp * comp;
};

/* Count of server locks */
static int camlidl_num_server_locks = 0;

/* Count of components */
extern int camlidl_num_components;

/* Handle for module (for the DLL) */
HANDLE camlidl_module_handle;

/* Methods of IClassFactory */

static HRESULT STDMETHODCALLTYPE
cfactory_QueryInterface(struct IClassFactory * self,
                        REFIID iid, void ** object)
{
  struct camlidl_factory * this = (struct camlidl_factory *) self;
  if (IsEqualIID(iid, &IID_IClassFactory) ||
      IsEqualIID(iid, &IID_IUnknown)) {
    *object = (void *) this;
    InterlockedIncrement(&(this->refcount));
    return S_OK;
  } else {
    *object = NULL;
    return E_NOINTERFACE;
  }
}

static ULONG STDMETHODCALLTYPE
cfactory_AddRef(struct IClassFactory * self)
{
  struct camlidl_factory * this = (struct camlidl_factory *) self;
  return InterlockedIncrement(&(this->refcount));
}

static ULONG STDMETHODCALLTYPE
cfactory_Release(struct IClassFactory * self)
{
  struct camlidl_factory * this = (struct camlidl_factory *) self;
  ULONG res = InterlockedDecrement(&(this->refcount));
  if (res == 0) free(this);
  return res;
}

static HRESULT STDMETHODCALLTYPE
cfactory_CreateInstance(struct IClassFactory * self,
                        interface IUnknown * outer,
                        REFIID iid, void ** object)
{
  struct camlidl_factory * this = (struct camlidl_factory *) self;
  value vcomp;
  interface IUnknown * comp;
  HRESULT res;

  /* Aggregation is not supported yet */
  if (outer != NULL) return CLASS_E_NOAGGREGATION;
  /* Create the component */
  vcomp = callback(Field(this->comp->compdata, COMPDATA_CREATE), Val_unit);
  comp = camlidl_unpack_interface(vcomp, NULL);
  /* Get the requested interface */
  res = comp->lpVtbl->QueryInterface(comp, iid, object);
  /* Release the initial pointer to the component
     (if QueryInterface failed, it will destroy itself) */
  comp->lpVtbl->Release(comp);
  /* Return result of QueryInterface */
  return res;
}

static HRESULT STDMETHODCALLTYPE
cfactory_LockServer(struct IClassFactory * self, BOOL block)
{
  if (block)
    InterlockedIncrement(&camlidl_num_server_locks);
  else
    InterlockedDecrement(&camlidl_num_server_locks);
  return S_OK;
}

static struct IClassFactoryVtbl cfactory_vtbl = {
  cfactory_QueryInterface,
  cfactory_AddRef,
  cfactory_Release,
  cfactory_CreateInstance,
  cfactory_LockServer
};

static struct camlidl_factory * cfactory_new(struct camlidl_comp * comp)
{
  struct camlidl_factory * f = malloc(sizeof(struct camlidl_factory));
  if (f == NULL) return NULL;
  f->lpVtbl = &cfactory_vtbl;
  f->refcount = 1;
  f->comp = comp;
  return f;
}

/* The class factory server */

STDAPI DllGetClassObject(REFCLSID clsid, REFIID iid, void ** object)
{
  struct camlidl_comp * c;
  struct camlidl_factory * f;
  HRESULT res;

  for (c = registered_components; c != NULL; c = c->next) {
    if (IsEqualCLSID(clsid,
                     (CLSID *) String_val(Field(c->compdata, COMPDATA_CLSID))))
    {
      /* Create class factory */
      f = cfactory_new(c);
      if (f == NULL) return E_OUTOFMEMORY;
      /* Get requested interface */
      res = f->lpVtbl->QueryInterface((struct IClassFactory *) f, iid, object);
      /* Release the class factory; if QueryInterface failed, it will free
         itself */
      f->lpVtbl->Release((struct IClassFactory *) f);
      /* Return result of QueryInterface */
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
  CLSID * clsid;
  HRESULT retcode;

  for (c = registered_components; c != NULL; c = c->next) {
    retcode =
      RegisterServer(
        camlidl_module_handle,
        (CLSID *) String_val(Field(c->compdata, COMPDATA_CLSID)),
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
  CLSID * clsid;
  HRESULT retcode;

  for (c = registered_components; c != NULL; c = c->next) {
    retcode =
      Unregisterserver(
        (CLSID *) String_val(Field(c->compdata, COMPDATA_CLSID)),
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
    camlidl_module_handle = module;
    caml_startup(argv);
    break;
  /* TODO: free all memory when DLL detached */
  }
  return TRUE;
}

#endif
