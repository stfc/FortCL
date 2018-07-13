program fortcl_query
  use ocl_utils_mod, only: init_device, CL_UTIL_STR_LEN
  use iso_c_binding
  implicit none
  character(len=CL_UTIL_STR_LEN) :: version_str
  integer(c_intptr_t) :: device, context

  call init_device(device, version_str, context)

end program fortcl_query
