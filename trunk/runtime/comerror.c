/* Error handling */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include "camlidlruntime.h"
#include "comstuff.h"

static void camlidl_raise_error(HRESULT errcode, char * who, char * msg)
{
  static value * com_error_exn = NULL;
  value bucket, vwho = Val_unit, vmsg = Val_unit;

  if (com_error_exn == NULL) {
    com_error_exn = caml_named_value("Com.Error");
    if (com_error_exn == NULL)
      invalid_argument("Exception Com.Error not initialized");
  }
  Begin_roots2(vwho,vmsg)
    vwho = copy_string(who);
    vmsg = copy_string(msg);
    bucket = alloc_small(4, 0);
    Field(bucket, 0) = *com_error_exn;
    Field(bucket, 1) = Val_long(errcode);
    Field(bucket, 3) = vwho;
    Field(bucket, 2) = vmsg;
  End_roots();
  mlraise(bucket);
}

void camlidl_error(HRESULT errcode, char * who, char * what)
{
  char msg[1024];

  if (what == NULL) {
#ifdef _WIN32
    if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
                      NULL,       /* message source */
                      errcode,
                      0,          /* language */
                      msg,        /* message buffer */
                      sizeof(msg),/* max size */
                      NULL)       /* inserts */
        != 0)
      what = msg;
    else
      what = "Unknown error";
#else
    what = "Unknown error";
#endif
  }
  camlidl_raise_error(errcode, who, what);
}

static void camlidl_hresult_error(HRESULT errcode)
{
  /* Build text representation of errcode */
#ifdef _WIN32
  interface IErrorInfo * errinfo = NULL;
  BSTR source, descr;
  char who[1024], what[1024];

  /* Try to use GetErrorInfo */
  GetErrorInfo(0L, &errinfo);
  if (errinfo != NULL) {
    errinfo->lpVtbl->GetSource(errinfo, &source);
    _snprintf(who, sizeof(who) - 1, "%ls", source);
    who[sizeof(who) - 1] = 0;
    errinfo->lpVtbl->GetDescription(errinfo, &descr);
    _snprintf(what, sizeof(who) - 1, "%ls", descr);
    what[sizeof(what) - 1] = 0;
    SysFreeString(source);
    SysFreeString(descr);
    errinfo->lpVtbl->Release(errinfo);
    camlidl_error(errcode, who, what);
  } else {
    camlidl_error(errcode, "", NULL);
  }
#else
  camlidl_error(errcode, "", NULL);
#endif
}

void camlidl_check_hresult(HRESULT hr)
{
  if (FAILED(hr)) camlidl_hresult_error(hr);
}

value camlidl_c2ml_hresult_bool(HRESULT hr)
{
  if (FAILED(hr)) camlidl_hresult_error(hr);
  return Val_bool(hr == S_OK);
}

void camlidl_ml2c_hresult_bool(value v, HRESULT * hr)
{
  *hr = Bool_val(v) ? S_OK : S_FALSE;
}

value camlidl_c2ml_hresult_int(HRESULT hr)
{
  if (FAILED(hr)) camlidl_hresult_error(hr);
  return Val_int(HRESULT_CODE(hr));
}

void camlidl_ml2c_hresult_int(value v, HRESULT * hr)
{
  *hr = MAKE_HRESULT(SEVERITY_SUCCESS, FACILITY_NULL, Int_val(v) & 0xFFFF);
}

HRESULT camlidl_result_exception(char * methname, value exn_bucket)
{
  /* TODO: clever mapping of exception to HRESULTS ? */
#ifdef _WIN32
  interface ICreateErrorInfo * createrr;
  interface IErrorInfo * errinfo;
  int wstrlen;
  wchar_t * wstr;
  char * exndesc;

  if (SUCCEEDED(CreateErrorInfo(&createrr))) {
    wstrlen = strlen(methname);
    wstr = (wchar_t *) malloc((wstrlen + 1) * sizeof(wchar_t));
    if (wstr != NULL) {
      mbstowcs(wstr, methname, wstrlen);
      createrr->lpVtbl->SetSource(createrr, wstr);
      free(wstr);
    }
    exndesc = format_caml_exception(exn_bucket);
    if (exndesc != NULL) {
      wstrlen = strlen(exndesc);
      wstr = (wchar_t *) malloc((wstrlen + 1) * sizeof(wchar_t));
      if (wstr != NULL) {
        mbstowcs(wstr, methname, wstrlen);
        createrr->lpVtbl->SetDescription(createrr, wstr);
        free(wstr);
      }
      free(exndesc);
    }
    if (SUCCEEDED(createrr->lpVtbl->QueryInterface(createrr,
                                                   &IID_IErrorInfo,
                                                   (void **) &errinfo))) {
      SeetErrorInfo(0L, errinfo);
      errinfo->lpVtbl->Release(errinfo);
    }
    createrr->lpVtbl->Release(createrr);
  }
#endif
  return MAKE_HRESULT(SEVERITY_ERROR, FACILITY_ITF, 0x200);
}

void camlidl_uncaught_exception(char * methname, value exn_bucket)
{
  char * msg = format_caml_exception(exn_bucket);
  fprintf(stderr, "Uncaught exception in COM method %s: %s\n",
          methname, msg);
  free(msg);
  exit(2);
}

#ifdef _WIN32

struct camlidl_sei {
  struct ISupportErrorInfoVtbl * lpVtbl;
  long refcount;
  struct camlidl_intf * intf;
};

HRESULT STDMETHODCALLTYPE
camlidl_sei_QueryInterface(struct ISupportErrorInfo * self,
                           REFIID iid, void ** object)
{
  return
    camlidl_QueryInterface(((struct camlidl_sei *)self)->intf, iid, object);
}

ULONG STDMETHODCALLTYPE
camlidl_sei_AddRef(struct ISupportErrorInfo * self)
{
  return InterlockedIncrement(&(((struct camlidl_sei *)self)->refcount));
}

ULONG STDMETHODCALLTYPE
camlidl_sei_Release(struct ISupportErrorInfo * self)
{
  struct camlidl_sei * s = (struct camlidl_sei *) self;
  ULONG newrefcount = InterlockedDecrement(&(s->refcount));
  if (newrefcount == 0) {
    struct IUnknown * i = (struct IUnknown *) (s->intf);
    i->lpVtbl->Release(i);
    stat_free(self);
  }
  return newrefcount;
}

HRESULT STDMETHODCALLTYPE
camlidl_sei_InterfaceSupportsErrorInfo(struct ISupportErrorInfo * self,
                                       REFIID iid)
{
  if (IsEqualIID(iid, &IID_IUnknown) ||
      IsEqualIID(iid, &IID_ISupportErrorInfo))
    return S_FALSE;
  else
    return S_OK;
}

static struct ISupportErrorInfoVtbl camlidl_sei_vtbl = {
  VTBL_PADDING
  camlidl_sei_QueryInterface,
  camlidl_sei_AddRef,
  camlidl_sei_Release,
  camlidl_sei_InterfaceSupportsErrorInfo
};

struct ISupportErrorInfo * camlidl_support_error_info(struct camlidl_intf * i)
{
  struct camlidl_sei * r =
    (struct camlidl_sei *) stat_alloc(sizeof(struct camlidl_sei));
  r->lpVtbl = &camlidl_sei_vtbl;
  r->refcount = 1;
  r->intf = i;
  ((struct IUnknown *) i)->lpVtbl->AddRef((struct IUnknown *) i);
  return (struct ISupportErrorInfo *) r;
}

#endif
