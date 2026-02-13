program derived_component_section_01
  implicit none

  type t2
    integer :: x
  end type

  type t1
    integer :: n
    type(t2), allocatable :: a(:)
  end type

  type(t1) :: v(1)
  integer :: tmp(1)

  v(1)%n = 775
  allocate(v(1)%a(1))
  v(1)%a(1)%x = -13

  tmp = v(:)%n
  if (tmp(1) /= 775) error stop "wrong value in derived component section"
  if (.not. allocated(v(1)%a)) error stop "allocatable component unexpectedly deallocated"
  if (v(1)%a(1)%x /= -13) error stop "allocatable component data got corrupted"
end program derived_component_section_01
