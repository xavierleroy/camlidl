/***********************************************************************/
/*                                                                     */
/*                              CamlIDL                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id: camlidlcompat.h,v 1.3 2000-08-19 11:05:01 xleroy Exp $ */

/* Compatibility macros to share IDL source between CamlIDL and MIDL */

#ifdef CAMLIDL

/* Define away MIDL-specific attributes */
#define local
#define endpoint
#define version(x)
#define transmit_as(x)
#define implicit_handle
#define auto_handle
#define callback

/* Define away OLE/Automation attributes */
#define bindable
#define custom(x,y)
#define defaultbind
#define defaultcollelem
#define defaultvalue(x)
#define displaybind
#define dual
#define helpcontext(x)
#define helpstring(x)
#define helpstringcontext(x)
#define hidden
#define id(x)
#define immediatebind
#define lcid
#define nonbrowsable
#define nonextensible
#define odl
#define oleautomation
#define optional
#define readonly
#define replaceable
#define requestedit
#define restricted
#define retval
#define source
#define uidefault
#define usesgetlasterror
#define vararg

#else

/* Define away CamlIDL-specific attributes */
#define abstract
#define c2ml(x)
#define errorcheck(x)
#define errorcode
#define ml2c(x)
#define mltype(x)
#define mlname(x)
#define null_terminated
#define bigarray
#define camlint
#define int32
#define int64
#define nativeint
#define fortran
#define managed
#define int_default(x)
#define long_default(x)

#endif
