F90 ?= gfortran


KERNELS = vadddevice.cl

OCL_TARGET ?= JIT # Options: XILINX_FPGA | INTEL_FPGA | JIT

# PLATFORM ?= xilinx_u200_xdma_201830_2
# EXECUTION_TARGET ?= hw_emu # Options: sw_emu | hw_emu | hw
# OCLBINS ?= $(KERNELS:.cl=.xclbin)
# PROFILING_FLAGS=--profile_kernel

# TO RUN with sw_emu or hw_emu use:
# $ emconfigutil --platform xilinx_u200_xdma_201830_2 --nd 1
# $ XCL_EMULATION_MODE=sw_emu ./vadd.exe 
#
# TODO: Add Intel FPGA environment
# TODO: Add Intel CPU environment

all: vadd

vadd: fortcl vaddhost.o $(OCLBINS)
	@echo $(OCL_TARGET) $(OCLBINS)
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
		${PROFILING_FLAGS} $<.xo -o $@


# Compile Libraries
fortcl:
	${MAKE} -C ../../

clean:
	rm -rf _x *.log *.o *~ *.exe *.mod *.info *.xo *.xclbin .run/ .Xil/

allclean: clean
	${MAKE} -C ../../ allclean
