all:
	${MAKE} -C src

clean:
	${MAKE} -C src clean

allclean: clean
	${MAKE} -C src allclean
