################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
CMD=	copygb2
FOPTS=	-O2 -r8 -fp-model strict -traceback -axcore-avx2 -qopenmp
LOPTS=	
INCS=	-I${G2_INCd}
OBJS=	copygb2.o
LIBS=  ${G2_LIBd} ${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd} \
       ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} -mkl
################################################################################
$(CMD):	$(OBJS)
	ifort $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)
#$(OBJS):	$(INCS)
copygb2.o:	copygb2.f
	ifort -c $(INCS) $(FOPTS) $*.f
clean:	
	-rm -f $(OBJS)
