/* Load or emulate COM-related includes */

#ifdef _WIN32

#include <wtypes.h>
#include <winbase.h>
#include <objbase.h>

#else

/* Emulation for other OS */

interface IUnknown;

struct IUnknownVtbl {
  DECLARE_VTBL_PADDING
  HRESULT (*QueryInterface)(interface IUnknown * this,
                            IID * iid, void ** object);
  ULONG (*AddRef)(interface IUnknown * this);
  ULONG (*Release)(interface IUnknown * this);
};

interface IUnknown {
  struct IUnknownVtbl * lpVtbl;
};

#define IsEqualIID(a,b) (memcmp(a, b, sizeof(IID)) == 0)
#define InterlockedIncrement(p) (++(*(p)))
#define InterlockedDecrement(p) (--(*(p)))
extern IID IID_IUnknown;
#define S_OK 0
#define S_TRUE S_OK
#define S_FALSE 1
#define E_NOINTERFACE 0x80004002
#define FAILED(hr) ((hr) & 0x80000000)
#define HRESULT_CODE(hr) ((hr) & 0xFFFF)
#define SEVERITY_FAILURE 1
#define SEVERITY_SUCCESS 0
#define FACILITY_NULL 0
#define FACILITY_ITF 4
#define MAKE_HRESULT(s,f,c) (((s) << 31) | ((f) << 16) | (c))

#endif
