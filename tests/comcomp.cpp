// A simple COM component, as per the book

#include <stdio.h>
#include <stddef.h>
#include <string.h>

#ifdef _WIN32

#include <objbase.h>

extern "C" {
IID IID_IX = { 0, 0, 0, { 0, 0, 0, 0, 0, 0, 0, 0x81 } };
IID IID_IY = { 0, 0, 0, { 0, 0, 0, 0, 0, 0, 0, 0x82 } };
}

#else

#define interface class
typedef struct { unsigned char data[16]; } IID;
typedef int HRESULT;
typedef unsigned long ULONG;
#define IsEqualIID(a,b) (memcmp(&a, &b, sizeof(IID)) == 0)
#define InterlockedIncrement(p) (++(*(p)))
#define InterlockedDecrement(p) (--(*(p)))
#define S_OK 0
#define E_NOINTERFACE (-1)
#define STDMETHODCALLTYPE 

extern "C" {
IID IID_IUnknown =
  { { 0, 0, 0, 0, 0, 0, 0xC0, 0, 0, 0, 0, 0, 0, 0, 0x46 } };
IID IID_IX = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x81 } };
IID IID_IY = { { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x82 } };
}

interface IUnknown {
 public:
  virtual HRESULT QueryInterface(const IID& iid, void ** res) = 0;
  virtual ULONG AddRef() = 0;
  virtual ULONG Release() = 0;
};
#endif

interface IX : public IUnknown {
 public:
  virtual void STDMETHODCALLTYPE F(int x) = 0;
};

interface IY : public IUnknown {
 public:
  virtual int STDMETHODCALLTYPE G(int x) = 0;
  virtual int STDMETHODCALLTYPE H() = 0;
  virtual int STDMETHODCALLTYPE K(char ** str) = 0;
};

static int CA_ident = 0;

class CA : public IX, public IY {

private:
  long refcount;
  int ident;
public:

  virtual HRESULT STDMETHODCALLTYPE QueryInterface(const IID& iid, void ** res) {
    if (IsEqualIID(iid, IID_IUnknown)) {
      printf("%d: QueryInterface: return IUnknown pointer.\n", ident);
      *res = (interface IX *) this;
    } else if (IsEqualIID(iid, IID_IX)) {
      printf("%d: QueryInterface: return IX pointer.\n", ident);
      *res = (interface IX *) this;
    } else if (IsEqualIID(iid, IID_IY)) {
      printf("%d: QueryInterface: return IY pointer.\n", ident);
      *res = (interface IY *) this;
    } else {
      printf("%d: QueryInterface: interface not supported.\n", ident);
      *res = NULL;
      return E_NOINTERFACE;
    }
    ((IUnknown *)(*res))->AddRef();
    return S_OK;
  }

  virtual ULONG STDMETHODCALLTYPE AddRef() {
    ULONG res = InterlockedIncrement(&refcount);
    printf("%d: AddRef: new refcount is %lu\n", ident, res);
    return res;
  }

  virtual ULONG STDMETHODCALLTYPE Release() {
    ULONG res = InterlockedDecrement(&refcount);
    printf("%d: Release: new refcount is %lu\n", ident, res);
    if (res == 0) {
      printf("%d: destroying component.\n", ident);
      delete this;
    }
    return res;
  }

  virtual void STDMETHODCALLTYPE F(int x) {
    printf("%d: F(%d) called.\n", ident, x);
  }

  virtual int STDMETHODCALLTYPE G(int x) {
    int res = 3 * x + 1;
    printf("%d: G(%d) called, returning %d.\n", ident, x, res);
    return res;
  }

  virtual int STDMETHODCALLTYPE H() {
    printf("%d: H() called, returning 0.\n", ident);
    return 0;
  }

  virtual int STDMETHODCALLTYPE K(char ** str) {
    printf("%d: K() called, returning 0 and `foobar'.\n", ident);
    *str = "foobar";
    return 0;
  }

  // constructor:
  CA() { refcount = 0; ident = ++CA_ident; }

  // destructor:
  ~CA() {
    printf("%d: destroy self.\n", ident);
  }
};

extern "C"
interface IUnknown * create_instance()
{
  interface IUnknown * res = (interface IX *) new CA();
  res->AddRef();
  return res;
}

extern "C"
void test_component(interface IUnknown * c)
{
  interface IX * cix;
  interface IY * ciy;
  int res;
  char * stringres;

  // Test IX interface
  if (c->QueryInterface(IID_IX, (void **) &cix) == S_OK) {
    printf("test: got IX interface.\n");
    printf("test: calling F(12) on it.\n");
    cix->F(12);
    printf("test: releasing the IX interface.\n");
    res = cix->Release();
    printf("test: return of Release() is %d.\n", res);
  }
  // Test IY interface
  if (c->QueryInterface(IID_IY, (void **) &ciy) == S_OK) {
    printf("test: got IY interface.\n");
    printf("test: calling G(3) on it.\n");
    res = ciy->G(3);
    printf("test: return value is %d.\n", res);
    printf("test: calling H() on it.\n");
    res = ciy->H();
    printf("test: return value is %d.\n", res);
    printf("test: calling K() on it.\n");
    res = ciy->K(&stringres);
    printf("test: hresult is %d, return string is `%s'.\n", res, stringres);
    printf("test: releasing the IY interface.\n");
    res = ciy->Release();
    printf("test: return of Release() is %d.\n", res);
  }
}

extern "C"
void test_ix(interface IX * c)
{
  test_component(c);
  //  printf("test: releasing the interface, return of Release() is %d.\n",
  //         c->Release());
}

extern "C"
void test_iy(interface IY * c)
{
  test_component(c);
  //  printf("test: releasing the interface, return of Release() is %d.\n",
  //         c->Release());
}


#if 0
extern "C"
int main(int argc, char ** argv)
{
  interface IUnknown * i = create_instance();
  test_component(i);
  i->Release();
  return 0;
}
#endif
