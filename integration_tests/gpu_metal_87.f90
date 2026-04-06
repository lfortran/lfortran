module m_gpu_metal_87
  implicit none
  type :: inner_t
  end type
  type :: outer_t
    type(inner_t), allocatable :: items(:)
  end type
end module

! Test: do concurrent inside associate block with array-of-structs variable
! where the struct has allocatable array members of another struct type.
! Previously caused ICE in the gpu_offload pass because __data_ and __size_
! kernel parameters were incorrectly created for array-of-structs variables.
program gpu_metal_87
  use m_gpu_metal_87
  implicit none
  type(outer_t) :: x(2)
  real :: a(4)
  integer :: i
  allocate(x(1)%items(1))
  allocate(x(2)%items(1))
  associate(n => size(x))
    do concurrent (i = 1:4)
      a(i) = real(n + i)
    end do
  end associate
  if (abs(a(1) - 3.0) > 0.001) error stop
  if (abs(a(2) - 4.0) > 0.001) error stop
  if (abs(a(3) - 5.0) > 0.001) error stop
  if (abs(a(4) - 6.0) > 0.001) error stop
  print *, "PASSED"
end program
