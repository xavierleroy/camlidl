/***********************************************************************/
/*                                                                     */
/*                              CamlIDL                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id: idlalloc.c,v 1.6 1999-03-15 15:21:40 xleroy Exp $ */

/* Allocation functions and various helper functions
   for stub code generated by camlidl */

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include "camlidlruntime.h"

/* Helper functions for conversion */

value camlidl_find_enum(int n, int *flags, int nflags, char *errmsg)
{
  int i;

  for (i = 0; i < nflags; i++) {
    if (n == flags[i]) return Val_int(i);
  }
  invalid_argument(errmsg);
  return Val_unit;              /* not reached, keeps CL happy */
}

value camlidl_alloc_flag_list(int n, int *flags, int nflags)
{
  value l = Val_int(0);
  int i;

  Begin_root(l)
    for (i = nflags - 1; i >= 0; i--)
      if (n & flags[i]) {
        value v = alloc_small(2, 0);
        Field(v, 0) = Val_int(i);
        Field(v, 1) = l;
        l = v;
        n &= ~ flags[i];
      }
  End_roots();
  return l;
}

mlsize_t camlidl_ptrarray_size(void ** array)
{
  mlsize_t i;

  for (i = 0; array[i] != NULL; i++) /*nothing*/;
  return i;
}

/* Malloc-like allocation with en masse deallocation */

void camlidl_register_allocation(camlidl_free_function free_fn,
                                 void * block,
                                 camlidl_ctx ctx)
{
  if (ctx->flags & CAMLIDL_TRANSIENT) {
    struct camlidl_block_list * l =
      stat_alloc(sizeof(struct camlidl_block_list));
    l->free_fn = free_fn;
    l->block = block;
    l->next = ctx->head;
    ctx->head = l;
  }
}

#ifdef _WIN32
static void camlidl_task_mem_free(void * ptr)
{
  CoTaskMemFree(ptr);
}
#endif

void * camlidl_malloc(size_t sz, camlidl_ctx ctx)
{
#ifdef _WIN32
  void * res = CoTaskMemAlloc(sz);
  if (res == NULL) raise_out_of_memory();
  camlidl_register_allocation(camlidl_task_mem_free, res, ctx);
#else
  void * res = stat_alloc(sz);
  camlidl_register_allocation(stat_free, res, ctx);
#endif
  return res;
}

void camlidl_free(camlidl_ctx ctx)
{
  struct camlidl_block_list * arena, * tmp;
  for (arena = ctx->head; arena != NULL; /*nothing*/) {
    arena->free_fn(arena->block);
    tmp = arena;
    arena = arena->next;
    stat_free(tmp);
  }
}

char * camlidl_malloc_string(value mlstring, camlidl_ctx ctx)
{
  mlsize_t len = string_length(mlstring);
  char * res = camlidl_malloc(len + 1, ctx);
  memcpy(res, String_val(mlstring), len + 1);
  return res;
}

/* This function is for compatibility with OCaml 2.00 and earlier */

#if defined(CAMLVERSION) && CAMLVERSION < 201

value camlidl_alloc (mlsize_t wosize, tag_t tag)
{
  value result;
  mlsize_t i;

  Assert (wosize > 0);
  if (wosize <= Max_young_wosize){
    result = alloc (wosize, tag);
    if (tag < No_scan_tag){
      for (i = 0; i < wosize; i++) Field (result, i) = 0;
    }
  }else{
    result = alloc_shr (wosize, tag);
    if (tag < No_scan_tag) memset (Bp_val (result), 0, Bsize_wsize (wosize));
    result = check_urgent_gc (result);
  }
  return result;
}

#endif
