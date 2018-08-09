all:
	${MAKE} -C src

# Target to build stub library (i.e. with no actual
# OpenCL functionality or dependence)
stub:
	${MAKE} -C src stub

clean:
	${MAKE} -C src clean

allclean: clean
	${MAKE} -C src allclean
