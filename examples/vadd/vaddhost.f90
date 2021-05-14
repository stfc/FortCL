
! Depending on the OpenCL Platform selected at compile time (see Makefile)
! a FILENAME with the appropriate string value is chosen.
! This can be an OpenCL source file or a compiled binary file.
#ifdef JIT
#define FILENAME "vadddevice.cl"
#elif INTEL_FPGA
#define FILENAME "vadddevice.aocx"
#elif XILINX_FPGA
#define FILENAME "vadddevice.xclbin"
#endif

program vadd
  use fortcl
  use clfortran
  use ocl_utils_mod, only: check_status
  use iso_c_binding
  implicit none

  integer, parameter :: N = 100
  integer(c_intptr_t), target :: deviceA, deviceB, deviceC
  integer(c_intptr_t), target :: vaddkernel
  integer(c_intptr_t), pointer :: cmd_queues(:)
  integer(c_size_t), target :: globalsize(1) = (/N/)
  real, dimension(N), target :: hostA, hostB, hostC, AfromDevice

  integer(c_size_t) :: arraysize = c_sizeof(hostA)
  integer(c_size_t), parameter :: zero = 0
  integer :: i, ierr

  ! Initialize B and C with random numbers
  call RANDOM_NUMBER(hostB)
  call RANDOM_NUMBER(hostC)

  ! Perform host vector add
  hostA = hostB + hostC


  ! Initialise the OpenCL device and environment
  call ocl_env_init()
  cmd_queues => get_cmd_queues()

  ! Allocate buffers on the device
  deviceA = create_wonly_buffer(arraysize)
  deviceB = create_ronly_buffer(arraysize)
  ! deviceC is only read, but we use rw_buffer to test FortCL API
  deviceC = create_rw_buffer(arraysize)

  ! Load the kernel and set up the arguments
  call add_kernels(1, "vadd", FILENAME, compiler_flags="-cl-fast-relaxed-math")
  vaddkernel = get_kernel_by_name("vadd")
  ierr = clSetKernelArg(vaddkernel, 0, C_SIZEOF(deviceB), C_LOC(deviceB))
  call check_status('clSetKernelArg: arg 1 of vadd', ierr)
  ierr = clSetKernelArg(vaddkernel, 1, C_SIZEOF(deviceC), C_LOC(deviceC))
  call check_status('clSetKernelArg: arg 2 of vadd', ierr)
  ierr = clSetKernelArg(vaddkernel, 2, C_SIZEOF(deviceA), C_LOC(deviceA))
  call check_status('clSetKernelArg: arg 3 of vadd', ierr)

  ! Copy B and C to the device
  ierr = clEnqueueWriteBuffer(cmd_queues(1), deviceB, CL_TRUE, zero, &
        arraysize, C_LOC(hostB), 0, C_NULL_PTR, C_NULL_PTR)
  call check_status('enqueue write buffer B', ierr)
  ierr = clEnqueueWriteBuffer(cmd_queues(1), deviceC, CL_TRUE, zero, &
       arraysize, C_LOC(hostC), 0, C_NULL_PTR, C_NULL_PTR)
  call check_status('enqueue write buffer C', ierr)
  ierr = clFinish(cmd_queues(1))
  call check_status('clFinish', ierr)


  ! Execute kernel
   ierr = clEnqueueNDRangeKernel(cmd_queues(1), vaddkernel, 1, &
      C_NULL_PTR, C_LOC(globalsize), C_NULL_PTR, 0, C_NULL_PTR, C_NULL_PTR)
  call check_status('NDRangeKernel', ierr)
  ierr = clFinish(cmd_queues(1))
  call check_status('clFinish', ierr)

  ! Copy A from the device
  ierr = clEnqueueReadBuffer(cmd_queues(1), deviceA, CL_TRUE, zero, &
       arraysize, C_LOC(AfromDevice), 0, C_NULL_PTR, C_NULL_PTR)
  call check_status('Read buffer', ierr)
  ierr = clFinish(cmd_queues(1))
  call check_status('clFinish', ierr)

  ! Clean up OpenCL device
  call ocl_release()


  ! Verify results
  do i = 1, N
    if (abs(hostA(i) - AfromDevice(i)) > 0.0001) then
        write(*,*) "Verification error: Number in possition ", i, & 
            " does not match (", hostA(i), AfromDevice(i),")"
        stop
    endif
  enddo

  write(*,*) "TEST PASSED!"

end program vadd
