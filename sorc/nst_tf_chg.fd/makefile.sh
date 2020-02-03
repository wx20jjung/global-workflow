#!/bin/sh

export FFLAGS="-O2 -fp-model strict -traceback -g -r8 -i4"
# for debugging
#export FFLAGS="-g -r8 -i4 -warn unused -check bounds"

#export NETCDF_INCLUDE="-I${NETCDF}/include"
export NETCDF_INCLUDE="-I${NETCDF_INC}"
export NETCDF_LDFLAGS_F="-L${NETCDF_LIB} -lnetcdf -lnetcdff"

make clean
make build
err=$?
if [ $err -ne 0 ]; then
  echo ERROR BUILDING nst_tf_chg      
  exit 2
fi
make install

exit
