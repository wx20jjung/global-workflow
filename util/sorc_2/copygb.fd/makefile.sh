#!/bin/sh
set -x
currdir=$(pwd)
cd ../../../
export FCMP=ifort
cd $currdir
make -f makefile
