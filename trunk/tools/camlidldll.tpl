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

#* $Id: camlidldll.tpl,v 1.4 2000-08-21 12:55:02 xleroy Exp $

# Automates the creation of a DLL for a Caml component

camllib='%%CAMLLIB%%'
output=caml.dll
linkopts=''
camlopts=''
linkobjs=''
camlobjs=''
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
    -cclib) lib=`echo $2 | sed -e 's/^-l\(.*\)$/lib\1.lib/'`
        linkobjs="$linkobjs $lib"
        shift;;
    -linkall|-verbose)
        camlopts="$camlopts $1";;
# other options
    -*) echo "Unknown option \"$1\", ignored" 1>&2;;
# files
    *.cm[oa])
        camlobjs="$camlobjs $1";;
    *.obj|*.lib)
        linkobjs="$linkobjs $1";;
    *.tlb)
        tlbcounter=`expr $tlbcounter + 1`
        echo "$tlbcounter typelib $1" >> $resourcefile;;
    *)  echo "Don't know what to do with \"$1\", ignored" 1>&2;;
  esac
  shift
done

if test $tlbcounter -ne 0; then
  echo "1 num_typelibs { $tlbcounter }" >> $resourcefile
  resfile="caml$$.res"
  rc /fo$resfile $resourcefile || { exit $?; }
  rm -f $resourcefile
fi

ocamlc -custom -output-obj -o $camlobjfile $camlopts com.cma $camlobjs && \
link /nologo /incremental:no /dll /machine:ix86 \
  /out:${output} /libpath:$camllib \
  /export:DllGetClassObject,@2,PRIVATE \
  /export:DllCanUnloadNow,@3,PRIVATE \
  /export:DllRegisterServer,@4,PRIVATE \
  /export:DllUnregisterServer,@5,PRIVATE \
  $resfile \
  $linkopts $camlobjfile $linkobjs \
  ${camllib}\\cfactory.obj libcamlidl.lib \
  libcamlrun.lib advapi32.lib ole32.lib oleaut32.lib
exitcode=$?
rm -f $resfile $camlobjfile
exit $exitcode
