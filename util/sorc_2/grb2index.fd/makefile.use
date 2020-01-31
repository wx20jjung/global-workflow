################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	grb2index
FOPTS= -O2 -fp-model strict -axcore-avx2 -traceback -qopenmp
LOPTS=	
INCS=	
OBJS=	grb2index.o
LIBS=	${G2_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} -mkl
################################################################################
$(CMD):	$(OBJS)
	ifort $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	makefile $(INCS)
grb2index.o:	grb2index.f
	ifort -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
