# Makefile for OpenCL utility modules.

# We don't include clfortran.o in the list of objects
# because we don't want it in the final archive file.
OBJECTS = ocl_params_mod.o ocl_utils_mod.o ocl_env_mod.o
LIB = libFortCL.a

all: ${LIB}

${LIB}: clfortran ${OBJECTS}
	${AR} ${ARFLAGS} ${LIB} ${OBJECTS}

clfortran:
	${F90} -c clfortran.f90

%.o: %.f90
	${F90} ${F90FLAGS} -c $<

clean:
	rm -f *.o
	rm -f *~

allclean: clean
	rm -f *.mod *.MOD
	rm -f ${LIB}
