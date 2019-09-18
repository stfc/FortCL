!> Module containing stub implementations of FortCL routines. Allows
!! the code to be compiled in the absence of an OpenCL implementation.
module fortcl
  use iso_c_binding, only: c_intptr_t, c_int32_t, c_int64_t, c_size_t
  use ocl_params_mod, only: wp
  implicit none

  private

  !> Pointer to the OpenCL device being used
  integer(c_intptr_t) :: cl_device
  !> The OpenCL context
  integer(c_intptr_t) :: cl_context

  public ocl_env_init
  public cl_context, cl_device, get_num_cmd_queues, get_cmd_queues
  public create_rw_buffer, add_kernels, get_kernel_by_name, read_buffer

contains

  !===================================================

  !> Initialise the GOcean environment
  subroutine ocl_env_init(num_queues)
    integer, intent(in), optional :: num_queues
    write (*,*) "FortCL compiled without OpenCL support!"
    cl_device = 0
    cl_context = 0
    return
  end subroutine ocl_env_init

  !===================================================

  subroutine add_kernels(nkernels, kernel_names, filename)
    use iso_c_binding, only: c_intptr_t
    integer, intent(in) :: nkernels
    character(len=*), intent(in) :: kernel_names(nkernels)
    character(len=*), intent(in), optional :: filename
    return
  end subroutine add_kernels

  !===================================================

  function get_kernel_by_name(name) result(kern)
    integer(c_intptr_t), target :: kern
    character(len=*), intent(in) :: name
    kern = 0
  end function get_kernel_by_name

  !===================================================

  function get_kernel_index(name) result(index)
    integer :: index
    character(len=*), intent(in) :: name
    index = 0
  end function get_kernel_index

  !===================================================

  function get_num_cmd_queues() result(num)
    integer :: num
    num = 0
  end function get_num_cmd_queues

  !===================================================

  function get_cmd_queues() result(queues)
    integer(c_intptr_t), pointer :: queues(:)
    queues => null()
  end function get_cmd_queues

  !===================================================

  !> Create a read-write buffer in our existing OpenCL context
  function create_rw_buffer(nbytes) result(buffer)
    integer(c_size_t), intent(in) :: nbytes
    integer(c_intptr_t), target :: buffer
    buffer = 0
  end function create_rw_buffer
  
  !===================================================

  subroutine read_buffer(device, host, nelem)
    integer(kind=c_intptr_t), intent(in) :: device
    real(kind=wp), intent(inout), target :: host(:,:)
    integer(8), intent(in) :: nelem
    return
  end subroutine read_buffer
  
  !===================================================

  subroutine ocl_release()
    return
  end subroutine ocl_release

end module fortcl

! ==============================================================
! PSyclone als uses the check_status function of ocl_utils_mod.
! So provide a stub implementation in a separate module as well:
module ocl_utils_mod

  contains

  subroutine check_status(text, ierr)
    implicit none
    character(len=*), intent(in) :: text
    integer, intent(in) :: ierr
  end subroutine check_status

end module ocl_utils_mod
