.SUFFIXES: .f90

SRC=rf.f90 statics.f90 calc_reg.f90

OBJS=$(SRC:.f90=.o)

FC=gfortran
#FC=ifort
#FFLAGS=-assume byterecl
#FCFLAGS=-g


all: $(OBJS)

.f90.o:
	$(FC) $(FCFLAGS) -c $< $(FCFLAGS) -o $@

clean:
	/bin/rm -f *.o *~

