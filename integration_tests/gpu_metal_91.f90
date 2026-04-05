! Test: calling a function with an element of an array-of-structs
! where the struct has an allocatable array member, inside do concurrent.
! The function accesses the allocatable member of the struct argument.
module gpu_metal_91_m
  implicit none
  type :: t
    real, allocatable :: v(:)
  end type
contains
  pure function get_first(x) result(r)
    type(t), intent(in) :: x
    real :: r
    r = x%v(1)
  end function
end module

program gpu_metal_91
  use gpu_metal_91_m
  implicit none
  type(t) :: a(2)
  real :: r(2)
  integer :: i
  a(1) = t([1.0])
  a(2) = t([2.0])
  do concurrent (i = 1:2)
    r(i) = get_first(a(i))
  end do
  if (abs(r(1) - 1.0) > 1e-6 .or. abs(r(2) - 2.0) > 1e-6) error stop
  print *, "PASS"
end program
