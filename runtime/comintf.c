/* Helper functions for handling COM interfaces */

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include "camlidlruntime.h"
#include "comstuff.h"

int camlidl_num_components = 0;

value camlidl_lookup_method(char * name)
{
  static value * lookup_clos = NULL;

  if (lookup_clos == NULL) {
    lookup_clos = caml_named_value("Oo.new_method");
    if (lookup_clos == NULL) invalid_argument("Oo.new_method not registered");
  }
  return callback(*lookup_clos, copy_string(name));
}

static void camlidl_finalize_interface(value intf)
{
  interface IUnknown * i = (interface IUnknown *) Field(intf, 1);
  i->lpVtbl->Release(i);
}

value camlidl_pack_interface(void * intf)
{
  value res = alloc_final(2, camlidl_finalize_interface, 0, 1);
  Field(res, 1) = (value) intf;
  return res;
}

void * camlidl_unpack_interface(value vintf)
{
  return (void *) Field(vintf, 1);
}

value camlidl_make_interface(void * vtbl, value caml_object, IID * iid)
{
  struct camlidl_component * comp =
    (struct camlidl_component *) stat_alloc(sizeof(struct camlidl_component));
  comp->numintfs = 1;
  comp->refcount = 1;
  comp->intf[0].vtbl = vtbl;
  comp->intf[0].caml_object = caml_object;
  comp->intf[0].iid = iid;
  comp->intf[0].comp = comp;
  register_global_root(&(comp->intf[0].caml_object));
  InterlockedIncrement(&camlidl_num_components);
  return camlidl_pack_interface(&(comp->intf[0]));
}

/* Basic methods (QueryInterface, AddRef, Release) for COM objects
   encapsulating a Caml object */

HRESULT camlidl_QueryInterface(struct camlidl_intf * this, IID * iid,
                               void ** object)
{
  struct camlidl_component * comp = this->comp;
  int i;
  for (i = 0; i < comp->numintfs; i++) {
    if (comp->intf[i].iid != NULL && IsEqualIID(iid, comp->intf[i].iid)) {
      *object = (void *) &(comp->intf[i]);
      InterlockedIncrement(&(comp->refcount));
      return S_OK;
    }
  }
  if (IsEqualIID(iid, &IID_IUnknown)) {
    *object = (void *) this;
    InterlockedIncrement(&(comp->refcount));
    return S_OK;
  }
  *object = NULL;
  return E_NOINTERFACE;
}
  
ULONG camlidl_AddRef(struct camlidl_intf * this)
{
  return InterlockedIncrement(&(this->comp->refcount));
}

ULONG camlidl_Release(struct camlidl_intf * this)
{
  struct camlidl_component * comp = this->comp;
  ULONG newrefcount = InterlockedDecrement(&(comp->refcount));
  int i;

  if (newrefcount == 0) {
    for (i = 0; i < comp->numintfs; i++)
      remove_global_root(&(comp->intf[i].caml_object));
    stat_free(comp);
    InterlockedDecrement(&camlidl_num_components);
  }
  return newrefcount;
}

/* Query a COM interface */

value camlidl_com_queryInterface(value vintf, value viid)
{
  void * res;
  interface IUnknown * intf =
    (interface IUnknown *) camlidl_unpack_interface(vintf);
  if (string_length(viid) != 16) failwith("badly formed IID");
  if (intf->lpVtbl->QueryInterface(intf, (IID *) String_val(viid),
                                   &res) != S_OK)
    failwith("interface not supported");
  return camlidl_pack_interface(res);
}

/* Combine the interfaces of two Caml components */

#define is_a_caml_interface(i) \
  ((void *) (((interface IUnknown *) i1)->lpVtbl->QueryInterface) == \
   (void *) camlidl_QueryInterface)

value camlidl_com_combine(value vintf1, value vintf2)
{
  struct camlidl_intf * i1, * i2;
  struct camlidl_component * c1, * c2, * c;
  int n, i;

  i1 = camlidl_unpack_interface(vintf1);
  i2 = camlidl_unpack_interface(vintf2);
  if (! is_a_caml_interface(i1) || ! is_a_caml_interface(i2))
    failwith("Com.combine: not a Caml interface");
  c1 = i1->comp;
  c2 = i2->comp;
  n = c1->numintfs + c2->numintfs;
  c = (struct camlidl_component *)
        stat_alloc(sizeof(struct camlidl_component) +
                   sizeof(struct camlidl_intf) * (n - 1));
  InterlockedIncrement(&camlidl_num_components);
  c->numintfs = n;
  c->refcount = 1;
  for (i = 0; i < c1->numintfs; i++)
    c->intf[i] = c1->intf[i];
  for (i = 0; i < c2->numintfs; i++)
    c->intf[c1->numintfs + i] = c2->intf[i];
  for (i = 0; i < n; i++) {
    register_global_root(&(c->intf[i].caml_object));
    c->intf[i].comp = c;
  }
  return camlidl_pack_interface(c->intf + (i1 - c1->intf));
}

/* Create an instance of a component */

value camlidl_com_create_instance(value clsid, value iid)
{
#ifdef _WIN32
  void * instance;
  HRESULT res;
  res = CoCreateInstance((CLSID *) String_val(clsid),
                         NULL,
                         CLSCTX_ALL,
                         (IID *) String_val(iid),
                         &instance);
  if (FAILED(res)) raise_com_error(res);
  return camlidl_pack_interface(instance);
#else
  invalid_argument("Com.create_instance not implemented");
#endif
}

/* Initialization, termination */

value camlidl_com_initialize(value unit)
{
#ifdef _WIN32
  CoInitialize();
#endif
  return Val_unit;
}

value camlidl_com_uninitialize(value unit)
{
#ifdef _WIN32
  CoUninitialize();
#endif
  return Val_unit;
}

