module ocl_utils_mod
  use clfortran
  use iso_c_binding
  implicit none

  integer, parameter :: CL_UTIL_STR_LEN = 64
  logical :: verbose

  !> This interface lets us over-load read_buffer to take Fortran
  !! arrays of differing ranks.
  !! \TODO support more than just 2D arrays
  interface read_buffer
     module procedure read_buffer2d
  end interface read_buffer

contains
  
  !> Initialise an OpenCL Device and Context using the platform given by the
  !! FORTCL_PLATFORM environment variable (defaults to 1 if not specified),
  !! and the device specified by the device_selection argument (defaults to
  !! 1 if not specified). Note that a device_selection set to 0 requests
  !! to select all available devices in the platform.
  subroutine init_device(device, version_str, context, device_selection)
    integer(c_intptr_t), intent(out) :: device, context 
    character(len=CL_UTIL_STR_LEN), intent(out) :: version_str
    integer, intent(in), optional :: device_selection
    ! Locals
    integer :: iplatform, idevice, iallocerr, selected_device
    integer(c_intptr_t), target :: ctx_props(3)
    integer(c_int32_t), target :: device_cu
    integer(c_size_t) :: iret, zero_size = 0
    integer(c_int32_t) :: ierr, num_devices, num_platforms, iignore
    integer(c_intptr_t), allocatable, target :: &
       platform_ids(:), device_ids(:)
    character(kind=c_char), allocatable, target :: device_name(:)
    character(len=1) :: strvalue = ' '
    ! Set verbosity to false if FORTCL_VERBOSE does not exist (ierr is 1) or
    ! is equal to "0".
    CALL get_environment_variable("FORTCL_VERBOSE", strvalue, status=ierr)
    if (ierr .eq. 1 .or. strvalue .eq. "0") then
        verbose = .False.
    else
        verbose = .True.
    endif

    num_platforms = 0
    ierr = clGetPlatformIDs(0, C_NULL_PTR, num_platforms)
    call check_status('clGetPlatformIDs', ierr)
    if (num_platforms < 1)then
       write (*,*) "Failed to get any OpenCL platform IDs"
       stop
    end if
    print '(a,i2)','Number of OpenCL Platforms: ',num_platforms

    allocate(platform_ids(num_platforms), stat=iallocerr)
    if (iallocerr.ne.0) stop 'memory allocation error'

    ! whenever "&" appears in C subroutine (address-of) call,
    ! then C_LOC has to be used in Fortran
    ierr = clGetPlatformIDs(num_platforms, C_LOC(platform_ids), iignore)
    call check_status('clGetPlatformIDs', ierr)

    ! Select the OpenCL platform to use
    CALL get_environment_variable("FORTCL_PLATFORM", strvalue, status=ierr)
    if(ierr .eq. 1) then
        ! By default use platform 1
        iplatform = 1
    else
        read(strvalue,"(i1)", iostat=ierr) iplatform
        if(ierr .ne. 0) then
            stop 'Error: Cannot convert FORTCL_PLATFORM value into an integer.'
        endif
        if(iplatform > num_platforms .or. iplatform <= 0) then
            write(*,*) 'Error: FORTCL_PLATFORM should be a number between ', &
                '1 and the number of platforms. Use `clinfo` to list the ', &
                'available OpenCL platforms in the system.'
            stop
        endif
    endif

    ! Get device IDs only for the selected platform
    num_devices = 0
    ierr=clGetDeviceIDs(platform_ids(iplatform), CL_DEVICE_TYPE_ALL, &
                        0, C_NULL_PTR, num_devices)
    call check_status('clGetDeviceIDs', ierr)
    if (num_devices < 1)then
       stop 'Failed to find any OpenCL devices'
    end if
    print '(a,i2)','Number of OpenCL Devices: ',num_devices

    allocate(device_ids(num_devices), stat=iallocerr)
    if (iallocerr.ne.0) stop 'memory allocation error'

    ! whenever "&" appears in C subroutine (address-off) call,
    ! then C_LOC has to be used in Fortran
    ierr = clGetDeviceIDs(platform_ids(iplatform), CL_DEVICE_TYPE_ALL, &
                          num_devices, C_LOC(device_ids), iignore)
    call check_status('clGetDeviceIDs', ierr)

    if (.not.present(device_selection)) then
        selected_device = 1
    else
        selected_device = device_selection
    endif
    ! Get device info from all devices
    do idevice=1, num_devices

       ierr=clGetDeviceInfo(device_ids(idevice), CL_DEVICE_MAX_COMPUTE_UNITS, &
                            sizeof(device_cu), C_LOC(device_cu), iret)
       call check_status('clGetDeviceInfo', ierr)
       ierr=clGetDeviceInfo(device_ids(idevice), CL_DEVICE_NAME, zero_size, &
                            C_NULL_PTR, iret)
       call check_status('clGetDeviceInfo', ierr)

       allocate(device_name(iret), stat=iallocerr)
       if (iallocerr.ne.0) stop 'allocate'

       ierr=clGetDeviceInfo(device_ids(idevice), CL_DEVICE_NAME, &
                            sizeof(device_name), C_LOC(device_name), iret)
       call check_status('clGetDeviceInfo', ierr)

       write (*, '(a,i2,a,i3,a)', advance='no') &
          'Device (#', idevice,', Compute Units: ',device_cu,') - '
       write(*,*) device_name
       deallocate(device_name)
    enddo

    if (selected_device == 0) then
       print '(a,i2,a,i2)', 'Creating OpenCL Context for ', num_devices, &
          ' devices of platform ', iplatform
       device = device_ids(1)
       ctx_props(1) = CL_CONTEXT_PLATFORM
       ctx_props(2) = platform_ids(iplatform)
       ctx_props(3) = 0
       context = clCreateContext(C_LOC(ctx_props), num_devices, &
                                 C_LOC(device_ids), C_NULL_FUNPTR, &
                                 C_NULL_PTR, ierr)
       call check_status('clCreateContext', ierr)
    elseif (selected_device > 0 .and. selected_device <= num_devices) then
       print '(a,i1,a,i2)', 'Creating OpenCL Context for device #', &
                selected_device, ' of platform ', iplatform
       device = device_ids(selected_device)
       ctx_props(1) = CL_CONTEXT_PLATFORM
       ctx_props(2) = platform_ids(iplatform)
       ctx_props(3) = 0
       context = clCreateContext(C_LOC(ctx_props), 1, &
                                 C_LOC(device_ids(selected_device)), &
                                 C_NULL_FUNPTR, C_NULL_PTR, ierr)
       call check_status('clCreateContext', ierr)
    else
        write(*,'(a)') "Error! Failed to initialize the OpenCL context:"
        write(*,'(a,i1,a,a,i1,a)') "The device_selection parameter is '", &
            selected_device, "', but it must be a number between 0 ", &
            "and the number of the available devices (", num_devices , &
            ") in the selected platform."
        stop
    endif

  end subroutine init_device

  !===================================================

  subroutine release_context(context)
    integer(c_intptr_t), intent(inout) :: context 
    ! Locals
    integer(c_int32_t) :: ierr

    ierr=clReleaseContext(context)
    call check_status('clReleaseContext', ierr)

  end subroutine release_context

  !===================================================

  !> Loads and builds the OpenCL program. If the filename received has a
  ! '.cl' extension, it will load the source file and compile the program.
  ! If it is not a '.cl' file, it will assume it is a pre-compiled binary
  ! and attempt to link it.
  function get_program(context, device, version_str, filename, compiler_flags) result(prog)
    integer(c_intptr_t), target :: prog
    integer(c_intptr_t), intent(inout), target :: device, context 
    character(len=CL_UTIL_STR_LEN), intent(in) :: version_str, filename
    ! Locals
    type(c_ptr), target :: psource
    character(len=*), intent(in), optional :: compiler_flags
    character(len=1024) :: options
    character(len=3) :: extension
    character(len=1, kind=c_char), target :: retinfo(1:1024), c_options(1:1024)
    integer :: i, irec, iallocerr, last_dot
    integer(c_int32_t) :: ierr
    integer, parameter :: iunit=10
    integer(c_size_t), target :: binary_size, iret
    byte, dimension(:), allocatable, target :: buffer

    ! Get the filename extension
    last_dot = scan(trim(filename), ".", BACK= .true.)
    extension = filename(last_dot:)

    ! Open the kernels file in binary form
    open(iunit, file=filename, form='unformatted', access='stream', status='old', &
        action='read', iostat=ierr)
    if (ierr.ne.0) then
       write(*,*) 'Cannot open file: ', TRIM(filename), 'error code:', ierr
       stop
    end if

    ! Get the size in bytes and allocate a byte buffer
    inquire(iunit, SIZE=binary_size)
    allocate(buffer(binary_size), stat=iallocerr)
    if (iallocerr.ne.0) then
         write(*,*) 'Error: Allocation of ', binary_size, ' bytes buffer to ', &
             'store the ', TRIM(filename), ' contents failed with error ', iallocerr
        stop
    endif

    ! Read the kernels files
    read(iunit, iostat=ierr) buffer
    close(iunit)

    psource=C_LOC(buffer) ! pointer to source code

    ! Create the OpenCL program
    if (extension == ".cl") then
        ! If it has a .cl extension, it will be JIT'ed
        prog = clCreateProgramWithSource(context, 1, C_LOC(psource), &
                                         C_LOC(binary_size), ierr)
        call check_status('clCreateProgramWithSource', ierr)
    else
        ! It the extension is not .cl, we assume it is a binary
        prog = clCreateProgramWithBinary(context, 1, C_LOC(device), &
                                         C_LOC(binary_size), C_LOC(psource), &
                                         C_NULL_PTR, ierr)

        call check_status('clCreateProgramWithBinary', ierr)
    endif
    deallocate(buffer)

    ! Prepare compiling options
    options = ""
    if (present(compiler_flags)) then
        options = trim(options) // trim(compiler_flags)
    endif
    ! Convert the options to a NULL-terminated C-char string
    irec = len(trim(options))
    do i=1, irec
       c_options(i)=options(i:i)
    enddo
    c_options(irec+1) = C_NULL_CHAR

    ! Build the OpenCL Program
    ierr=clBuildProgram(prog, 0, C_NULL_PTR, C_LOC(c_options), &
                        C_NULL_FUNPTR,C_NULL_PTR)

    ! If compilation didn't succeed, print compilation errors and terminate
    if (ierr.ne.CL_SUCCESS) then
        print *, 'clBuildProgram', ierr
        print '(a)', ' *** Options *** '
        print '(1024a)', options(1:min(irec, 1024))
        ierr=clGetProgramBuildInfo(prog, device, CL_PROGRAM_BUILD_LOG, &
                                  sizeof(retinfo), C_LOC(retinfo),iret)
        if (ierr.ne.0) stop 'clGetProgramBuildInfo'
        print '(a)', 'build log start'
        print '(1024a)', retinfo(1:min(iret, 1024))
        print '(a)', 'build log end'
        stop
    endif

  end function get_program


  !> Release an OpenCL program object
  subroutine release_program(prog)
    integer(c_intptr_t), target, intent(inout) :: prog
    ! Locals
    integer(c_int32_t) :: ierr

    ierr = clReleaseProgram(prog)
    call check_status('clReleaseProgram', ierr)
  end subroutine release_program


  !> Get a kernel object from a program object
  !>
  !> @param [in] prog OpenCL program object obtained from
  !>                  <code>get_program</code>
  !> @return the kernel object
  !>
  function get_kernel(prog, kernel_name) result(kernel)
    integer(c_intptr_t), target, intent(in) :: prog
    character(len=*), intent(in) :: kernel_name
    integer(c_intptr_t), target :: kernel
    ! Locals
    integer :: irec, i
    character(len=1, kind=c_char), target :: c_kernel_name(1:1024)

    irec = len(trim(kernel_name))
    do i=1, irec
       c_kernel_name(i) = kernel_name(i:i)
    enddo
    c_kernel_name(irec+1) = C_NULL_CHAR

    kernel = clCreateKernel(prog, C_LOC(c_kernel_name), irec)
    call check_status('clCreateKernel: '//TRIM(kernel_name), irec)

  end function get_kernel

  !===================================================

  subroutine release_kernel(kern)
    integer(c_intptr_t), target, intent(inout) :: kern
    integer(c_int32_t) :: ierr

    ierr = clReleaseKernel(kern)
    call check_status('clReleaseKernel', ierr)

  end subroutine release_kernel

  !===================================================

  !> Set-up the specified number of OpenCL command queues for the specified
  !! context and device.
  subroutine init_cmd_queues(nqueues, queues, context, device, &
                             enable_profiling, out_of_order)
    !> The number of command queues to create
    integer, intent(in) :: nqueues
    integer(c_intptr_t), target, intent(inout) :: queues(nqueues)
    integer(c_intptr_t), intent(in) :: device
    integer(c_intptr_t), intent(in) :: context
    logical, intent(in), optional :: enable_profiling, out_of_order
    ! Locals
    integer :: i
    integer(c_int32_t) :: ierr
     ! Start with empty properties
    integer(c_int64_t) :: properties = INT(b'00', kind=c_int64_t)

    if (present(enable_profiling)) then
       if (enable_profiling) then
          properties = ior(properties, CL_QUEUE_PROFILING_ENABLE) 
       endif
    endif

    if (present(out_of_order)) then
       if (out_of_order) then
          properties = ior(properties, CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE)
       endif
    endif

    do i=1, nqueues
       queues(i) = clCreateCommandQueue(context, device, properties, ierr)
       call check_status('clCreateCommandQueue', ierr)
    end do

  end subroutine init_cmd_queues

  !===================================================

  subroutine release_cmd_queues(nqueues, queues)
    integer, intent(in) :: nqueues
    integer(c_intptr_t), target, intent(inout) :: queues(nqueues)
    ! Locals
    integer :: iq
    integer(c_int32_t) :: ierr

    do iq=1, nqueues
       ierr=clReleaseCommandQueue(queues(iq))
       call check_status('clReleaseCommandQueue', ierr)
    end do

  end subroutine release_cmd_queues

  !===================================================

  !> Create a buffer in the supplied OpenCL context
  function create_buffer(context, access, nbytes) result(buffer)
    integer(c_intptr_t), intent(in) :: context
    integer(c_int64_t), intent(in) :: access
    integer(c_size_t), intent(in) :: nbytes
    integer(c_intptr_t), target :: buffer
    ! Locals
    integer(c_int32_t) :: ierr

    buffer = clCreateBuffer(context, access,          &
                            nbytes, C_NULL_PTR, ierr)
    call check_status('clCreateBuffer', ierr)

  end function create_buffer

  !=====================================================

  !> Read a buffer for a 2D Fortran array from an OpenCL device.
  !! Only necessary to hide the need to get a pointer to the start
  !! of the array.
  subroutine read_buffer2d(queue, device_ptr, local_array, nelem)
    use ocl_params_mod, only: wp
    integer(c_intptr_t), intent(in) :: queue, device_ptr
    real(kind=wp), target, intent(inout) :: local_array(:,:)
    integer(8), intent(in) :: nelem
    ! Pass the first element of the array that will be filled with
    ! data from the buffer on the OpenCL device
    call read_buffer_ptr(queue, device_ptr, local_array(1,1), nelem)
  end subroutine read_buffer2d

  !> Read a buffer (containing 64-bit floats) from an OpenCL device. Call
  !! blocks until read is complete.
  subroutine read_buffer_ptr(queue, device_ptr, local_array, nelem)
    use ocl_params_mod, only: wp
    integer(c_intptr_t), intent(in) :: queue, device_ptr
    real(kind=wp), target, intent(in) :: local_array
    integer(8), intent(in) :: nelem
    ! Locals
    integer(c_int32_t) :: ierr
    integer(c_intptr_t), target :: event
    integer(8) :: nbytes

    nbytes = nelem * 8_8
    ierr = clEnqueueReadBuffer(queue, device_ptr, CL_TRUE, 0_8, &
                               nbytes, C_LOC(local_array),      &
                               0, C_NULL_PTR, C_LOC(event))
    call check_status('clEnqueueReadBuffer', ierr)

    !> \todo implement an asynchronous read so we don't have to wait
    !! for each one to complete.
    ierr = clWaitForEvents(1, C_LOC(event))
    call check_status('clWaitForEvents', ierr)

  end subroutine read_buffer_ptr

  !=====================================================

  !> Check the return code of an OpenCL API cal
  subroutine check_status(text, ierr)
    implicit none
    character(len=*), intent(in) :: text
    integer, intent(in) :: ierr


    if(ierr /= CL_SUCCESS)then
       write(*,'("Hit error: ",(A),": ",(A))') text, OCL_GetErrorString(ierr)
       stop
    end if
    if(verbose)then
       write(*,'("Called ",(A)," OK")') text 
    end if
  end subroutine check_status
  
function OCL_GetErrorString(error)
  implicit none
  character(len=64) :: OCL_GetErrorString
  integer, intent(in) :: error
  select case(error)

    case (CL_SUCCESS)
        OCL_GetErrorString = "CL_SUCCESS"
    case (CL_DEVICE_NOT_FOUND)
        OCL_GetErrorString = "CL_DEVICE_NOT_FOUND"
    case (CL_DEVICE_NOT_AVAILABLE)
        OCL_GetErrorString = "CL_DEVICE_NOT_AVAILABLE"
    case (CL_COMPILER_NOT_AVAILABLE)
        OCL_GetErrorString = "CL_COMPILER_NOT_AVAILABLE"
    case (CL_MEM_OBJECT_ALLOCATION_FAILURE)
        OCL_GetErrorString = "CL_MEM_OBJECT_ALLOCATION_FAILURE"
    case (CL_OUT_OF_RESOURCES)
        OCL_GetErrorString = "CL_OUT_OF_RESOURCES"
    case (CL_OUT_OF_HOST_MEMORY)
        OCL_GetErrorString = "CL_OUT_OF_HOST_MEMORY"
    case (CL_PROFILING_INFO_NOT_AVAILABLE)
        OCL_GetErrorString = "CL_PROFILING_INFO_NOT_AVAILABLE"
    case (CL_MEM_COPY_OVERLAP)
        OCL_GetErrorString = "CL_MEM_COPY_OVERLAP"
    case (CL_IMAGE_FORMAT_MISMATCH)
        OCL_GetErrorString = "CL_IMAGE_FORMAT_MISMATCH"
    case (CL_IMAGE_FORMAT_NOT_SUPPORTED)
        OCL_GetErrorString = "CL_IMAGE_FORMAT_NOT_SUPPORTED"
    case (CL_BUILD_PROGRAM_FAILURE)
        OCL_GetErrorString = "CL_BUILD_PROGRAM_FAILURE"
    case (CL_MAP_FAILURE)
        OCL_GetErrorString = "CL_MAP_FAILURE"
    case (CL_INVALID_VALUE)
        OCL_GetErrorString = "CL_INVALID_VALUE"
    case (CL_INVALID_DEVICE_TYPE)
        OCL_GetErrorString = "CL_INVALID_DEVICE_TYPE"
    case (CL_INVALID_PLATFORM)
        OCL_GetErrorString = "CL_INVALID_PLATFORM"
    case (CL_INVALID_DEVICE)
        OCL_GetErrorString = "CL_INVALID_DEVICE"
    case (CL_INVALID_CONTEXT)
        OCL_GetErrorString = "CL_INVALID_CONTEXT"
    case (CL_INVALID_QUEUE_PROPERTIES)
        OCL_GetErrorString = "CL_INVALID_QUEUE_PROPERTIES"
    case (CL_INVALID_COMMAND_QUEUE)
        OCL_GetErrorString = "CL_INVALID_COMMAND_QUEUE"
    case (CL_INVALID_HOST_PTR)
        OCL_GetErrorString = "CL_INVALID_HOST_PTR"
    case (CL_INVALID_MEM_OBJECT)
        OCL_GetErrorString = "CL_INVALID_MEM_OBJECT"
    case (CL_INVALID_IMAGE_FORMAT_DESCRIPTOR)
        OCL_GetErrorString = "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR"
    case (CL_INVALID_IMAGE_SIZE)
        OCL_GetErrorString = "CL_INVALID_IMAGE_SIZE"
    case (CL_INVALID_SAMPLER)
        OCL_GetErrorString = "CL_INVALID_SAMPLER"
    case (CL_INVALID_BINARY)
        OCL_GetErrorString = "CL_INVALID_BINARY"
    case (CL_INVALID_BUILD_OPTIONS)
        OCL_GetErrorString = "CL_INVALID_BUILD_OPTIONS"
    case (CL_INVALID_PROGRAM)
        OCL_GetErrorString = "CL_INVALID_PROGRAM"
    case (CL_INVALID_PROGRAM_EXECUTABLE)
        OCL_GetErrorString = "CL_INVALID_PROGRAM_EXECUTABLE"
    case (CL_INVALID_KERNEL_NAME)
        OCL_GetErrorString = "CL_INVALID_KERNEL_NAME"
    case (CL_INVALID_KERNEL_DEFINITION)
        OCL_GetErrorString = "CL_INVALID_KERNEL_DEFINITION"
    case (CL_INVALID_KERNEL)
        OCL_GetErrorString = "CL_INVALID_KERNEL"
    case (CL_INVALID_ARG_INDEX)
        OCL_GetErrorString = "CL_INVALID_ARG_INDEX"
    case (CL_INVALID_ARG_VALUE)
        OCL_GetErrorString = "CL_INVALID_ARG_VALUE"
    case (CL_INVALID_ARG_SIZE)
        OCL_GetErrorString = "CL_INVALID_ARG_SIZE"
    case (CL_INVALID_KERNEL_ARGS)
        OCL_GetErrorString = "CL_INVALID_KERNEL_ARGS"
    case (CL_INVALID_WORK_DIMENSION)
        OCL_GetErrorString = "CL_INVALID_WORK_DIMENSION"
    case (CL_INVALID_WORK_GROUP_SIZE)
        OCL_GetErrorString = "CL_INVALID_WORK_GROUP_SIZE"
    case (CL_INVALID_WORK_ITEM_SIZE)
        OCL_GetErrorString = "CL_INVALID_WORK_ITEM_SIZE"
    case (CL_INVALID_GLOBAL_OFFSET)
        OCL_GetErrorString = "CL_INVALID_GLOBAL_OFFSET"
    case (CL_INVALID_EVENT_WAIT_LIST)
        OCL_GetErrorString = "CL_INVALID_EVENT_WAIT_LIST"
    case (CL_INVALID_EVENT)
        OCL_GetErrorString = "CL_INVALID_EVENT"
    case (CL_INVALID_OPERATION)
        OCL_GetErrorString = "CL_INVALID_OPERATION"
    case (CL_INVALID_GL_OBJECT)
        OCL_GetErrorString = "CL_INVALID_GL_OBJECT"
    case (CL_INVALID_BUFFER_SIZE)
        OCL_GetErrorString = "CL_INVALID_BUFFER_SIZE"
    case (CL_INVALID_MIP_LEVEL)
        OCL_GetErrorString = "CL_INVALID_MIP_LEVEL"
    case(CL_INVALID_GLOBAL_WORK_SIZE)
        OCL_GetErrorString = "CL_INVALID_GLOBAL_WORK_SIZE"
    case default
        OCL_GetErrorString = "unknown error code"
     end select
   end function OCL_GetErrorString

end module ocl_utils_mod
