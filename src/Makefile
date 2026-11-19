# ====================================================================== #
#                                                                        #
#            Makefile for NETWORK                                        #
#                   2007.05.29 by NISHIMURA, Nobuya                      #
#                                                                        #
#            update log:                                                 #
#              2008.07.01                                                #
#                - modified for module build (N.N.)                      #
# ====================================================================== #

include config.mk

FCFLAGS ?= -O3

PRG := ../nse

MOD := $(patsubst %.f90,%.mod,$(wildcard mod_*.f90))
OBJ := $(patsubst %.f90,%.o,$(wildcard *.f90))

.PHONY: all nse clean
.DELETE_ON_ERROR:

all: $(PRG)

nse: $(PRG)

$(PRG): $(OBJ) Makefile config.mk
	$(FC) $(FCFLAGS) $(LDFLAGS) -o $@ $(OBJ) $(LIB) $(LDLIBS)

# Fortran module compilation produces the object and module file together.
# gfortran leaves an unchanged module file's timestamp intact.
mod_%.o mod_%.mod: mod_%.f90 config.mk
	$(FC) $(FCFLAGS) -c -o mod_$*.o $<
	touch -c mod_$*.mod

%.o: %.f90 config.mk
	$(FC) $(FCFLAGS) -c -o $@ $<

# Module prerequisites follow the USE statements in each source file.
mod_set.o mod_set.mod mod_neu.o mod_neu.mod func_coul.o: mod_const.mod
mod_ffn.o mod_ffn.mod mod_out.o mod_out.mod: mod_const.mod mod_set.mod

eq_nse.o ini_set.o new_raph.o nr_retry.o part_func.o: mod_const.mod mod_set.mod
neutrino.o: mod_set.mod mod_neu.mod
ofile.o therm.o: mod_set.mod
sstate.o: mod_const.mod mod_set.mod mod_wagoner.mod mod_ffn.mod
nse.o: mod_const.mod mod_set.mod mod_out.mod mod_ffn.mod

clean:
	$(RM) $(PRG) $(OBJ) $(MOD)
