#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"false"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
else
  if [ $target = s4 ] ; then
    export MOD_PATH=/data/prod/ncep_libs/intel/18.0.3/modulefiles
    source ./gfs_post.fd/modulefiles/post/v8.0.0-s4           > /dev/null 2>&1
  else
    export MOD_PATH=${cwd}/lib/modulefiles
  fi
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd gfs_post.fd/sorc
sh build_ncep_post.sh
