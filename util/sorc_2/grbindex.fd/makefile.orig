################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	grbindex
FOPTS=	
#FOPTS=	-O3   -qintsize=8 -qrealsize=8 
LOPTS=	-O2 -traceback -fp-model strict -axcore-avx2 -qopenmp
INCS=	
OBJS=	grbindex.o
LIBS =  ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} -mkl
################################################################################
$(CMD):	$(OBJS)
	ifort $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	makefile $(INCS)
grbindex.o:	grbindex.f
	ifort -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
