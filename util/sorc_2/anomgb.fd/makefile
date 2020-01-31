################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=  /bin/sh
CMD=    anomgb
FOPTS=  -O2 -r8 -fp-model strict -convert big_endian -traceback -qopenmp
#LOPTS=  -openmp
LOPTS=
INCS=
OBJS=   anomgb.o
LIBS=  -L ../../../lib ${W3EMC_LIBd} ${W3NCO_LIBd} ${IP_LIBd} ${SP_LIBd} ${BACIO_LIB4} -mkl
################################################################################
$(CMD): $(OBJS)
	ifort $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):        $(INCS)
anomgb.o:       anomgb.f
	mpif90 -c $(FOPTS) $*.f
clean:
	-rm -f $(OBJS)

