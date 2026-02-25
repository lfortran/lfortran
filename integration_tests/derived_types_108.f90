program derived_types_108
  implicit none

  type :: t
    integer, pointer :: p
  end type

  type(t) :: x
  integer :: v
  allocate(x%p)
  x%p = 1
  v = x%p
  print *, v
  if (v /= 1) error stop
  x%p = 42
  if (x%p /= 42) error stop
  v = x%p + 1
  if (v /= 43) error stop
  deallocate(x%p)
end program
