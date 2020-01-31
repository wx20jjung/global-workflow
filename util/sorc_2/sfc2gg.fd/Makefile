MV       = mv -f
RM       = rm -f
FC	     = ifort

ifneq ($(wildcard /nwprod/lib/.),)
# WCOSS
COREINC	 = /nwprod/lib/incmod
CORELIB	 = /nwprod/lib
else
ifneq ($(wildcard /contrib/nceplibs/nwprod/lib/.),)
# ZEUS
COREINC	 = /contrib/nceplibs/nwprod/lib/incmod
CORELIB	 = /contrib/nceplibs/nwprod/lib
else
ifneq ($(wildcard /scratch3/NCEPDEV/nwprod/lib/.),)
# THEIA
COREINC	 = /scratch3/NCEPDEV/nwprod/lib/incmod
CORELIB	 = /scratch3/NCEPDEV/nwprod/lib
endif
endif
endif


FOPTS	 = -free -O2 -fp-model strict -traceback
LOPTS	 =
INCS	 = -I$(SFCIO_INC4) -I$(W3EMC_INCd) ${W3NCO_INCd}
LIBS	 = ${BACIO_LIB4} ${W3EMC_LIBd} ${W3NCO_LIBd} ${SP_LIB4} ${SFCIO_LIB4} \
           -L -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -liomp5

all: sfc2gg
neat:
	$(RM) sfc2gg.o
clean: neat
	$(RM) sfc2gg
install:
	$(MV) sfc2gg ../../exec/
sfc2gg: sfc2gg.o
	$(FC) $(INCS) $(FOPTS) $(LOPTS) sfc2gg.o $(LIBS) -o sfc2gg
sfc2gg.o: sfc2gg.f
	$(FC) $(INCS) $(FOPTS) -c sfc2gg.f
