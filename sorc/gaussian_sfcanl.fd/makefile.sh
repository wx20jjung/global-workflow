#!/bin/sh

export FFLAGS="-O2 -fp-model strict -g -r8 -i4 -traceback"
# for debugging
#export FFLAGS="-g -r8 -i4 -warn unused -check bounds"

#export NETCDF_INCLUDE="-I${NETCDF}/include"
export NETCDF_INCLUDE="-I${NETCDF_INC}"
#export NETCDF_LDFLAGS_F="-L${NETCDF}/lib -lnetcdf -lnetcdff -L${HDF5}/lib -lhdf5 -lhdf5_fortran"
export NETCDF_LDFLAGS_F="-L${NETCDF_LIB} -lnetcdf -lnetcdff"

make clean
make build
err=$?
if [ $err -ne 0 ]; then
  echo ERROR BUILDING GAUSSIAN_SFCANL
  exit 2
fi
make install

exit
