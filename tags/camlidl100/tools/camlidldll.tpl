#!/bin/sh
# This is a Bash script 
# Automates the creation of a DLL for a Caml component

camllib='%%CAMLLIB%%'
output=caml.dll
linkopts=''
camlopts=''
linkobjs=''
camlobjs=''

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
    *)  echo "Don't know what to do with \"$1\", ignored" 1>&2;;
  esac
  shift
done

ocamlc -custom -output-obj -o camlidldll.obj $camlopts $camlobjs && \
link /nologo /incremental:no /dll /out:${output} /libpath:$camllib \
  /export:DllGetClassObject,@2,PRIVATE \
  /export:DllCanUnloadNow,@3,PRIVATE \
  /export:DllRegisterServer,@4,PRIVATE \
  /export:DllUnregisterServer,@5,PRIVATE \
  $linkopts $linkobjs \
  ${camllib}\\cfactory.obj libcamlidl.lib \
  libcamlrun.lib ole32.lib oleaut32.lib advapi32.lib
