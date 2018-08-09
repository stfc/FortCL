# FortCL

Fortran wrapper library providing OpenCL functionality.
It uses (and includes) the "clFortran" interface code
available from https://github.com/cass-support/clfortran.

# Compiling #

The Makefile picks-up the compiler, flags etc. from environment
variables. You will need to set F90 (and optionally, F90FLAGS).
At link time you will need to link with the OpenCL run-time
libraries on your system.

# Example #

There is an example in the `examples` directory but in short:

    program fcl_test
      implicit none
      use iso_c_binding, only: c_intptr_t
      use fortcl
      character(len=*) :: filename = "/my/compiled/kernels.aocx"
      integer(c_intptr_t), target :: device_buffer
      ! Initialise the OpenCL device and environment
      call ocl_env_init()
      ! Load a kernel (expects pre-compiled kernels currently)
      call add_kernels(1, "my_ocl_kernel", filename)
      ! Create a read-write buffer on the device
      device_buffer = create_rw_buffer(num_bytes)
      ! ...
      ! OpenCL-specific code here to set kernel arguments and
      ! launch kernel.
      ! ...
      ! Clean up
      call ocl_release()
    end program fcl_test
