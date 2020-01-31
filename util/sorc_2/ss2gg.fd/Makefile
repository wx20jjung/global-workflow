MV       = mv -f
RM       = rm -f
FC	     = ifort

COREINC	 = ../../../lib/incmod
CORELIB	 = ../../../lib

INCsigio = $(COREINC)/sigio_4
INCw3    = $(COREINC)/w3emc_d

FOPTS	 = -free -O2 -traceback -fp-model strict
LOPTS	 =
INCS	 = -I$(SIGIO_INC4) -I$(W3EMC_INCd)
LIBS	 = ${BACIO_LIB4} ${W3EMC_LIBd} ${W3NCO_LIBd} ${SP_LIB4} ${SIGIO_LIB4} \
           -L -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -liomp5

all: ss2gg ss2lv sigdif
neat:
	$(RM) machine.o physcons_v.o ss2gg.o ss2lv.o sigdif.o machine.mod physcons.mod
clean: neat
	$(RM) ss2gg ss2lv sigdif
install:
	$(MV) ss2gg ss2lv sigdif ../../exec/
ss2gg: machine.o physcons_v.o ss2gg.o
	$(FC) $(INCS) $(FOPTS) $(LOPTS) ss2gg.o physcons_v.o machine.o $(LIBS) -o ss2gg
ss2lv: ss2lv.o
	$(FC) $(INCS) $(FOPTS) $(LOPTS) ss2lv.o physcons_v.o machine.o $(LIBS) -o ss2lv
ss2gg.o: ss2gg.f
	$(FC) $(INCS) $(FOPTS) -c ss2gg.f
ss2lv.o: ss2lv.f
	$(FC) $(INCS) $(FOPTS) -c ss2lv.f
sigdif: sigdif.o
	$(FC) $(INCS) $(FOPTS) $(LOPTS) sigdif.o physcons_v.o machine.o $(LIBS) -o sigdif
sigdif.o: sigdif.f
	$(FC) $(INCS) $(FOPTS) -c sigdif.f
machine.o: machine.f
	$(FC) $(FOPTS) -c machine.f
physcons_v.o: physcons_v.f
	$(FC) $(FOPTS) -c physcons_v.f
