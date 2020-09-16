all:
	${MAKE} -C src OCL=yes

# Target to build stub library (i.e. with no actual
# OpenCL functionality or dependence)
stub:
	${MAKE} -C src OCL=no

clean:
	${MAKE} -C src clean

allclean: clean
	${MAKE} -C src allclean
