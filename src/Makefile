# Makefile for FortCL.

F90 ?= gfortran

# Get the absolute path to the makefile
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
# Then get the absolute path of the makefile directory,
# which is the source directory.
ROOT  := $(dir $(mkfile_path))

# We don't include clfortran.o in the list of objects
# because we don't want it in the final archive file.
# (see ${LIB} target below)
OBJECTS = ocl_params_mod.o
ifeq ($(OCL),no)
	# Objects required to build the library using just stub interfaces
	# so as to avoid any actual dependence on an OpenCL installation.
	OBJECTS += FortCL_stub.o
else
	OBJECTS += ocl_utils_mod.o FortCL.o
endif

.PHONY: all clean

LIB = libFortCL.a

all: ${LIB}

${LIB}: clfortran.o ${OBJECTS}
	${AR} ${ARFLAGS} ${LIB} ${OBJECTS}

%.o: $(ROOT)/%.f90
	${F90} ${F90FLAGS} -c $<

clean:
	rm -f *.o
	rm -f *~

allclean: clean
	rm -f *.mod *.MOD
	rm -f ${LIB}
