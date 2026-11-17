program module_global_coarray_01
  use module_global_coarray_01_m, only: x
  implicit none
  interface
    subroutine module_global_coarray_01_set()
    end subroutine module_global_coarray_01_set
  end interface
  call module_global_coarray_01_set()
  if (x /= 2) error stop
end program module_global_coarray_01
