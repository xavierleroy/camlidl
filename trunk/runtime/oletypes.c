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

/* $Id: oletypes.c,v 1.2 2000-08-19 11:05:00 xleroy Exp $ */

/* Support for OLE Automation data types */

#include <string.h>
#include <wchar.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include "camlidlruntime.h"
#include "comstuff.h"
#include <oleauto.h>

static void camlidl_free_bstr(void * data)
{
  SysFreeString((BSTR) data);
}

/* Convert a Caml string to a BSTR */

void camlidl_ml2c_Com_BSTR(value s, BSTR * res, camlidl_ctx ctx)
{
  int len = mbstowcs(NULL, String_val(s), 0);
  BSTR bs = SysAllocStringLen(NULL, len);
  if (bs == NULL) raise_out_of_memory();
  camlidl_register_allocation(camlidl_free_bstr, bs, ctx);
  mbstowcs(bs, String_val(s), len);
  *res = bs;
}

/* Convert a BSTR to a Caml string */

value camlidl_c2ml_Com_BSTR(BSTR * bs, camlidl_ctx ctx)
{
  int len = wcstombs(NULL, *bs, 0);
  value res = alloc_string(len);
  wcstombs(String_val(res), *bs, len);
  return res;
}
