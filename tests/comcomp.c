/* A simple COM component, as per the book */

#include <stdio.h>
#include <stddef.h>
#include "comcomp.h"

typedef struct { unsigned char data[16]; } IID;
typedef int HRESULT;
typedef unsigned long ULONG;
#define IsEqualIID(a,b) (memcmp(a, b, sizeof(IID)) == 0)
#define InterlockedIncrement(p) (++(*(p)))
#define InterlockedDecrement(p) (--(*(p)))
#define S_OK 0
#define E_NOINTERFACE (-1)

IID IID_IUnknown = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80 } };
IID IID_IX = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x81 } };
IID IID_IY = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x82 } };

struct CA {
  struct IXVtbl * vtbl_ix;
  struct IYVtbl * vtbl_ix;
  int refcount;
  int ident;
};

#define CA_of_IX(i) ((struct CA *) (i))
#define CA_of_IY(i) ((struct CA *) (((void **) (i)) - 1))
#define IX_of_CA(i) ((struct IX *) (i))
#define IY_of_CA(i) ((struct IY *) (((void **) (i)) + 1))

ULONG CA_AddRef(struct CA * this)
{
  ULONG res = InterlockedIncrement(&this->refcount);
  printf("%d: AddRef: new refcount is %d\n", this->ident, res);
  return res;
}

ULONG CA_Release(struct CA * this) {
  ULONG res = InterlockedDecrement(&this->refcount);
  printf("%d: Release: new refcount is %d\n", this->ident, res);
  if (res == 0) {
    printf("%d: destroying component.\n", this->ident);
    free(this);
  }
  return res;
}

HRESULT CA_QueryInterface(struct CA * this, IID * iid, void ** res)
{
  if (IsEqualIID(iid, &IID_IUnknown)) {
    printf("%d: QueryInterface: return IUnknown pointer.\n", this->ident);
    *res = IX_of_CA(this);
  } else if (IsEqualIID(iid, &IID_IX)) {
    printf("%d: QueryInterface: return IX pointer.\n", ident);
    *res = IX_of_CA(this);
  } else if (IsEqualIID(iid, &IID_IY)) {
    printf("%d: QueryInterface: return IY pointer.\n", ident);
    *res = IY_of_CA(this);
  } else {
    printf("%d: QueryInterface: interface not supported.\n", ident);
    *res = NULL;
    return E_NOINTERFACE;
  }
  CA_AddRef(this);
  return S_OK;
}

HRESULT IX_QueryInterface(struct IX * this, IID * iid, void ** res)
{ return CA_QueryInterface(CA_of_IX(this), iid, res); }
ULONG IX_AddRef(struct IX * this)
{ return CA_AddRef(CA_of_IX(this)); }
ULONG IX_Release(struct IX * this)
{ return CA_Release(CA_of_IX(this)); }

void IX_F(struct IX * this, int x)
{
  printf("%d: F(%d) called.\n", this->ident, x);
}

HRESULT IY_QueryInterface(struct IY * this, IID * iid, void ** res)
{ return CA_QueryInterface(CA_of_IY(this), iid, res); }
ULONG IY_AddRef(struct IY * this)
{ return CA_AddRef(CA_of_IY(this)); }
ULONG IY_Release(struct IY * this)
{ return CA_Release(CA_of_IY(this)); }

int IY_G(struct IY * this, int x)
{
  int res = 3 * x + 1;
  printf("%d: G(%d) called, returning %d.\n", this->ident, x, res);
  return res;
}

struct IXVtbl IX_table = { IX_QueryInterface, IX_AddRef, IX_Release, IX_F };
struct IYVtbl IY_table = { IY_QueryInterface, IY_AddRef, IY_Release, IY_G };

struct CA * make_CA()
{
  struct CA * c = malloc(sizeof(struct CA));
  c->vtbl_ix = &IX_table;
  c->vtbl_iy = &IY_table;
  c->refcount = 0;
  c->ident = ++CA_ident;
  return c;
}

static void test_component(struct IUnknown * c)
{
  struct IX * cix;
  struct IY * ciy;
  int res;

  // Test IX interface
  if (c->lpVtbl->QueryInterface(c, &IID_IX, (void **) &cix) == S_OK) {
    printf("test: got IX interface.\n");
    printf("test: calling F(12) on it.\n");
    cix->lpVtbl->F(cix, 12);
    printf("test: releasing the IX interface.\n");
    res = cix->lpVtbl->Release(cix);
    printf("test: return of Release() is %d.\n", res);
  }
  // Test IY interface
  if (c->lpVtbl->QueryInterface(c, &IID_IY, (void **) &ciy) == S_OK) {
    printf("test: got IY interface.\n");
    printf("test: calling G(3) on it.\n");
    res = ciy->lpVtbl->G(ciy, 3);
    printf("test: return value is %d.\n", res);
    printf("test: releasing the IY interface.\n");
    res = ciy->lpVtbl->Release(ciy);
    printf("test: return of Release() is %d.\n", res);
  }
}

void test_ix(struct IX * c)
{
  test_component((struct IUnknown *) c);
}

void test_iy(struct IY * c)
{
  test_component((struct IUnknown *) c);
}

extern "C"
int main(int argc, char ** argv)
{
  struct IUnknown * i = create_instance();
  test_component(i);
  i->Release();
  return 0;
}
#endif
