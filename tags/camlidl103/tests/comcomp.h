extern IID IID_IUnknown, IID_IX, IID_IY;

struct IUnknown;

struct IUnknownVtbl {
  DECLARE_VTBL_PADDING
  HRESULT (*QueryInterface)(struct IUnknown * this, IID * iid, void ** res);
  ULONG (*AddRef)(struct IUnknown * this);
  ULONG (*Release)(struct IUnknown * this);
};

struct IUnknown {
  struct IUnknownVtbl * lpVtbl;
};

struct IX;

struct IXVtbl {
  DECLARE_VTBL_PADDING
  HRESULT (*QueryInterface)(struct IX * this, IID * iid, void ** res);
  ULONG (*AddRef)(struct IX * this);
  ULONG (*Release)(struct IX * this);
  void (*F)(struct IX * this, int x);
};

struct IX {
  struct IXVtbl * lpVtbl;
};

struct IY;

struct IYVtbl {
  DECLARE_VTBL_PADDING
  HRESULT (*QueryInterface)(struct IY * this, IID * iid, void ** res);
  ULONG (*AddRef)(struct IY * this);
  ULONG (*Release)(struct IY * this);
  int (*G)(struct IY * this, int x);
};

struct IY {
  struct IYVtbl * lpVtbl;
};

extern struct IUnknown * create_instance(void);
extern void test_component(struct IUnknown * c);
