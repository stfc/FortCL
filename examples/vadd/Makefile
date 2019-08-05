F90 ?= gfortran


KERNELS = vadddevice.cl

OCL_TARGET = XILINX_FPGA # Options: XILINX_FPGA | INTEL_FPGA | INTEL_CPU

ifeq ($(OCL_TARGET), XILINX_FPGA)
PLATFORM = xilinx_u200_xdma_201830_2
EXECUTION_TARGET = sw_emu # Options: sw_emu | hw_emu | hw
OCLBINS = $(KERNELS:.cl=.xclbin)
# TO RUN with sw_emu or hw_emu use:
# $ emconfigutil --platform xilinx_u200_xdma_201830_2 --nd 1
# $ XCL_EMULATION_MODE=sw_emu ./vadd.exe 
endif
ifeq ($(OCL_TARGET), INTEL_FPGA)
# TODO: Add Intel FPGA environment
endif
ifeq ($(OCL_TARGET), INTEL_CPU)
# TODO: Add Intel CPU environment
endif

all: vadd

vadd: fortcl vaddhost.o $(OCLBINS)
	${F90} vaddhost.o -L../../src/ -lFortCL ${OPENCL_LIBS} -lOpenCL \
		-o vadd.exe

%.o: %.f90
	${F90} ${F90FLAGS} -cpp -D${OCL_TARGET} -I../../src -c $<


# Xilinx OpenCL kernel compilation
%.xclbin: %.cl
	# Compile to a .xo file
	xocc --compile --platform ${PLATFORM} --target ${EXECUTION_TARGET} \
		$< -o $<.xo
	# Link to crete a Xilinx FPGA bianry container (.xclbin)
	xocc --link --platform ${PLATFORM} --target ${EXECUTION_TARGET} \
		$<.xo -o $@


# Compile Libraries
fortcl:
	${MAKE} -C ../../

clean:
	rm -rf _x *.log *.o *~ *.exe *.mod *.info *.xo *.xclbin

allclean: clean
	${MAKE} -C ../../ allclean
