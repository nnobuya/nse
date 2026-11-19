# ====================================================================== #
#                                                                        #
#            Makefile for NETWORK                                        #
#                   2007.05.29 by NISHIMURA, Nobuya                      #
#                                                                        #
#            update log:                                                 #
#              2008.07.01                                                #
#                - modified for module build (N.N.)                      #
# ====================================================================== #

#FC	:= ifort
FC	:= gfortran

FCFLAGS	:= -O3 #-check all -warn all

PRG	:= nse

MAIN	:= $(PRG:%=%.f90)

MODNAME	:= mod_const mod_set mod_wagoner mod_ffn mod_out mod_neu

SRCMOD	:= $(MODNAME:%=%.f90)

MOD	:= $(MODNAME:%=%.mod)

OBJMOD	:= $(MODNAME:%=%.o)

SRC	:= $(filter-out $(SRCMAIN) $(SRCMOD), $(wildcard *.f90))

OBJ	:= $(SRC:%.f90=%.o)

LIBPRG	:= lib$(PRG).a

LIBOBJ	:= $(OBJ:%.o=$(LIBPRG)(%.o))

LIBMOD	:= $(OBJMOD:%.o=$(LIBPRG)(%.o))

#=============================================================================

all:	$(PRG)

$(PRG):	%: %.f90 $(LIBPRG) $(LIBOBJ) $(LIBMOD)
	$(FC) $(FCFLAGS)  -o $@ $< -L. $(LIBPRG) $(LIB)

$(LIBPRG): $(LIBOBJ)

$(LIBOBJ): $(LIBMOD)

%.o: %.f90
	$(FC) -c $(FCFLAGS) $<

clean:
	rm -f *.o *~ $(LIBPRG) $(MOD) $(NTWK) $(PRG) fort.* *__*.*
