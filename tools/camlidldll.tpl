#!/bin/sh
# This is a Bash script 

#***********************************************************************
#*                                                                     *
#*                              CamlIDL                                *
#*                                                                     *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *
#*                                                                     *
#*  Copyright 1999 Institut National de Recherche en Informatique et   *
#*  en Automatique.  All rights reserved.  This file is distributed    *
#*  under the terms of the GNU Library General Public License.         *
#*                                                                     *
#***********************************************************************

#* $Id: camlidldll.tpl,v 1.6 2002-04-22 11:50:47 xleroy Exp $

# Automates the creation of a DLL for a Caml component

camllib='%%CAMLLIB%%'
output=caml.dll
linkopts=''
camlopts=''
linkobjs=''
camlobjs=''
camlnativeobjs=''
camlobjfile="caml$$.obj"
resourcefile="caml$$.rc"
resfile=''
tlbcounter=0

rm -f $resourcefile

# Parse the command line

while : ; do
  case "$1" in
    "") break;;
# my options
    -o) output=$2; shift;;
    -linkopt)
        linkopts="$linkopts $2"; shift;;
# ocamlc options
    -cc|-ccopt|-I|-w)
        camlopts="$camlopts $1 $2"; shift;;
    -cclib)
        lib=`echo $2 | sed -e 's/^-l\(.*\)$/lib\1.lib/'`
        linkobjs="$linkobjs $lib"
        shift;;
    -linkall|-verbose)
        camlopts="$camlopts $1";;
# other options
    -*) echo "Unknown option \"$1\", ignored" 1>&2;;
# files
    *.cm[oa])
        camlobjs="$camlobjs $1";;
    *.cmx|*.cmxa)
        camlnativeobjs="$camlnativeobjs $1";;
    *.obj|*.lib)
        linkobjs="$linkobjs $1";;
    *.tlb)
        tlbcounter=`expr $tlbcounter + 1`
        echo "$tlbcounter typelib $1" >> $resourcefile;;
    *)  echo "Don't know what to do with \"$1\", ignored" 1>&2;;
  esac
  shift
done

if test -n "$camlobjs" -a -n "$camlnativeobjs"; then
  echo "Both bytecode object files and native object files given, cannot proceed" 1>&2
  exit 2
fi

if test $tlbcounter -ne 0; then
  echo "1 num_typelibs { $tlbcounter }" >> $resourcefile
  resfile="caml$$.res"
  rc /fo$resfile $resourcefile || { exit $?; }
  rm -f $resourcefile
fi

if test -z "$camlnativeobjs"; then
  ocamlc -custom -output-obj -o $camlobjfile $camlopts com.cma $camlobjs
  exitcode=$?
  runtimelib=libcamlrun.lib
else
  ocamlopt -output-obj -o $camlobjfile $camlopts com.cmxa $camlnativeobjs
  exitcode=$?
  runtimelib=libasmrun.lib
fi

if test "$exitcode" -eq 0; then
  link /nologo /incremental:no /dll /machine:ix86 \
    /out:${output} /libpath:$camllib \
    /export:DllGetClassObject,PRIVATE \
    /export:DllCanUnloadNow,PRIVATE \
    /export:DllRegisterServer,PRIVATE \
    /export:DllUnregisterServer,PRIVATE \
    $resfile \
    $linkopts $camlobjfile $linkobjs \
    ${camllib}\\cfactory.obj libcamlidl.lib \
    $runtimelib \
    advapi32.lib ole32.lib oleaut32.lib
  exitcode=$?
fi

rm -f $resfile $camlobjfile
exit $exitcode
