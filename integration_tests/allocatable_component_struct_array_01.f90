module pkg_mod
  implicit none

  type :: variant_t
    integer, allocatable :: executable(:)
  end type variant_t

contains

  subroutine temp_func(src)
    type(variant_t), intent(in) :: src
    if (.not. allocated(src%executable)) error stop "expected allocated"
  end subroutine temp_func

end module pkg_mod

program allocatable_component_struct_array_01
  use pkg_mod
  implicit none

  type(variant_t) :: variants_array(2)
  integer :: j

  allocate(variants_array(1)%executable(1))
  variants_array(1)%executable(1) = 1

  call temp_func(variants_array(1))
  if (allocated(variants_array(2)%executable)) error stop "expected unallocated"

  do j = 1, size(variants_array)
     if (allocated(variants_array(j)%executable) .neqv. (j == 1)) error stop "allocation mismatch"
  end do
end program allocatable_component_struct_array_01
