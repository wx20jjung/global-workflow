################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	copygb
FOPTS=	-O2 -r8 -fp-model strict -convert big_endian -traceback -axcore-avx2 -qopenmp
LOPTS=	-qopenmp
INCS=	
OBJS=	copygb.o
#LIBS= -L/nwprod/lib -lw3_d -lbacio_4 -lip_d -lsp_d	
LIBS=   ${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} -mkl
################################################################################
$(CMD):	$(OBJS)
	mpif90 $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
$(OBJS):	$(INCS)
copygb.o:	copygb.f
	mpif90 -c $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
