/* Load or emulate COM-related includes */

#ifdef _WIN32

#include "objbase.h"

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
#define E_NOINTERFACE (-1)

#endif
