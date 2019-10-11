F90 ?= gfortran

# The OpenCL kernels will be compiled as part of the build system unless the
# JIT option is selected. In the JIT case this file will be ignored.
KERNELS = vadddevice.cl

# Options: JIT | XILINX_FPGA | INTEL_FPGA
# This value is passed as a DEFINE to the compiler and is it used in the source
# code for platform-specific sections (e.g. choosing the kernel filename).
OCL_TARGET ?= JIT 

# Uncomment and configure the environment variables below according
# to the selected OCL_TARGET:

# 1. JIT Target
# -------------
OCLBINS =  # No kernel will be compiled at compile time.

# To RUN, place the .cl file in the same folder and use:
# $ ./vadd.exe


# 2. XILINX FPGA
# --------------
# PLATFORM ?= xilinx_u200_xdma_201830_2
# EXECUTION_TARGET ?= hw_emu # Options: sw_emu | hw_emu | hw
# OCLBINS ?= $(KERNELS:.cl=.xclbin)
# XILINX_FLAGS='--optimize 3'
# XILINX_FLAGS=--profile_kernel

# TO RUN with sw_emu or hw_emu use:
# $ emconfigutil --platform xilinx_u200_xdma_201830_2 --nd 1
# $ XCL_EMULATION_MODE=sw_emu ./vadd.exe 
# TO RUN in the FPGA use:
# $ ./vadd.exe


# 3. Intel FPGA
# -------------
# TODO: Add Intel FPGA environment variables

all: vadd

vadd: fortcl vaddhost.o $(OCLBINS)
	@echo "Compiling for $(OCL_TARGET)"
	${F90} vaddhost.o -L../../src/ -lFortCL ${OPENCL_LIBS} -lOpenCL \
		-o vadd.exe

%.o: %.f90
	${F90} ${F90FLAGS} -cpp -D${OCL_TARGET} -I../../src -c $<


# Xilinx FPGA OpenCL kernel compilation
%.xclbin: %.cl
	# Compile to a .xo file
	xocc --compile --platform ${PLATFORM} --target ${EXECUTION_TARGET} \
		${XILINX_FLAGS} $< -o $<.xo
	# Link to crete a Xilinx FPGA bianry container (.xclbin)
	xocc --link --platform ${PLATFORM} --target ${EXECUTION_TARGET} \
		${XILINX_FLAGS} $<.xo -o $@

# Intel FPGA OpenCL kernel compilation
# TODO: Add Intel FPGA kernel compilation


# Compile Libraries
fortcl:
	${MAKE} -C ../../

clean:
	rm -rf _x *.log *.o *~ *.exe *.mod *.info *.xo *.xclbin .run/ .Xil/

allclean: clean
	${MAKE} -C ../../ allclean
