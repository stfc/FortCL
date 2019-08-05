F90 ?= gfortran


all: fortcl_query

fortcl_query: fortcl fortcl_query.o
	${F90} fortcl_query.o -L../../src/ -lFortCL ${OPENCL_LIBS} -lOpenCL \
-o fortcl_query.exe

fortcl:
	${MAKE} -C ../../

%.o: %.f90
	${F90} ${F90FLAGS} -I../../src -c $<

clean:
	rm -f *.o *~ *.exe *.mod

allclean: clean
	${MAKE} -C ../../ allclean
