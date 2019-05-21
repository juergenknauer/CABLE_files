# Makefile for CompileObservations
.SUFFIXES: .f90  .o 

VPATH	= ./
FC	= ifort                    # compiler

NCLIB   = $(NETCDF_ROOT)/lib/
NCINC   = $(NETCDF_ROOT)/include/
LIBS    = -lnetcdf -lnetcdff

#FFLAGS	=  -O2
FFLAGS = -O0 -fp-model precise -debug all -g -traceback
#FFLAGS	= -O0 -g -traceback -fpe:0 -check all -fastkchk
#FFLAGS = -check all,noarg_temp_created -warn all -g -debug -traceback -fp-stack-check -O0 -debug
LDFLAGS	=  $(FFLAGS)
#CFLAGS	='-O0 -fp-model precise -debug all -g  '

SRCS	= CompileObservations.f90
MODS	= Utils.f90 
	 
OBJS    =  ${MODS:.f90=.o} 
OBJS    += ${SRCS:.f90=.o} 

PROG	= "./CompileObservations.exe"

# GO

all:	$(SRCS) $(MODS) $(PROG)

$(PROG):	$(OBJS) 
	$(FC) $(LDLAGS) -L$(NCFLIB) -L$(NCCLIB) $(LIBS) $(OBJS) -o $@

%.o:	%.f90 $(MODS)	
	$(FC) $(FFLAGS) -I$(NCFINC) -I$(NCCINC) -c $<

clean:
	rm -f *.o $(PROG) *.mod

CompileObservations.o: Utils.o
