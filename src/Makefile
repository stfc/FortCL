# Makefile for FortCL.

# We don't include clfortran.o in the list of objects
# because we don't want it in the final archive file.
OBJECTS = ocl_params_mod.o ocl_utils_mod.o FortCL.o
LIB = libFortCL.a

all: ${LIB}

# Target to build the library using just stub interfaces so as
# to avoid any actual dependence on an OpenCL installation.
stub: ocl_params_mod.o FortCL_stub.o
	${AR} ${ARFLAGS} ${LIB} ocl_params_mod.o FortCL_stub.o

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
