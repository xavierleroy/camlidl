//
// Iface.h - 
//    Declarations of interfaces, IIDs, and CLSID
//    shared by the client and the component.
//
interface IX : IUnknown
{
	virtual HRESULT pascal Fx() = 0 ;
};


interface IY : IUnknown
{
	virtual HRESULT pascal Fy(int x) = 0 ;
};


interface IZ : IUnknown
{
	virtual HRESULT pascal Fz(int x, int * y) = 0 ;
};

//
// Declaration of GUIDs for interfaces and component.
//   These constants are defined in GUIDs.cpp.
//
extern "C" const IID IID_IX ;
extern "C" const IID IID_IY ;
extern "C" const IID IID_IZ ;

extern "C" const CLSID CLSID_Component1, CLSID_Component2 ;
