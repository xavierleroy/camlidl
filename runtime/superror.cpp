/* The ISupportErrorInfo interface for Caml components */

extern "C" {
#include <caml/mlvalues.h>
#include "camlidlruntime.h"
}
#include "comstuff.h"

class camlidl_sei : public ISupportErrorInfo
{
private:
  struct camlidl_intf * intf;
  long refcount;

public:

  virtual HRESULT __stdcall QueryInterface(const IID& iid, void ** object)
  {
    return camlidl_QueryInterface(intf, iid, object);
  }
  ULONG __stdcall AddRef()
  {
    return InterlockedIncrement(&refcount);
  }
  ULONG __stdcall Release()
  {
    ULONG newrefcount = InterlockedDecrement(&refcount);
    if (newrefcount == 0) {
      ((IUnknown *) intf)->Release();
      delete this;
    }
    return newrefcount;
  }
  HRESULT __stdcall InterfaceSupportsErrorInfo(const IID& iid)
  {
    if (iid == IID_IUnknown || iid == IID_ISupportErrorInfo)
      return S_FALSE;
    else
      return S_OK;
  }

  // constructor
  camlidl_sei (struct camlidl_intf * intf_init)
  {
    intf = intf_init;
    ((IUnknown *) intf)->AddRef();
    refcount = 1;
  }
};

extern "C"
ISupportErrorInfo *  camlidl_support_error_info(struct camlidl_intf * i)
{
  return new camlidl_sei(i);
}
