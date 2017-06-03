mkdir -p _build/src

path="_build/${@: -1}"

if [ ${path: -5} == ".byte" ];then
  ocamlc="-custom,"
else
  ocamlc=""
fi

corebuild -r -pkgs core,ipaddr,tuntap,async,cstruct,charrua-core.wire -Is src,test $*
# -lflags $ocamlc`pwd`/src/tuntap_stubs.c $*

if [[ "$path" =~ .*\.(byte|native)$ ]]; then
  name=`basename "$path"`
  rm "$name"
  mv "$path" ./
fi
