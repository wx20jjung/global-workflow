#!/bin/sh
set -x
currdir=$(pwd)
cd ../../../
export LIBDIR=$(pwd)/lib
export FCMP=ifort
cd $currdir
make -f makefile