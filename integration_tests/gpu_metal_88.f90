module m_gpu_metal_88
  implicit none
  type :: a_t
    integer :: x
  end type
  type :: b_t
    type(a_t), allocatable :: items(:)
  contains
    procedure :: n
  end type
contains
  pure integer function n(self)
    class(b_t), intent(in) :: self
    n = size(self%items)
  end function
end module

! Test: do concurrent calling a type-bound procedure on a struct
! that has an allocatable array member of another struct type.
! Previously failed at Metal shader compilation because the element
! type of the allocatable struct array was emitted as
! "/* unsupported struct type */" instead of the actual struct name.
program gpu_metal_88
  use m_gpu_metal_88
  implicit none
  type(b_t) :: b
  integer :: i
  real :: r(2)
  allocate(b%items(3))
  do concurrent (i = 1:2)
    r(i) = real(b%n())
  end do
  if (abs(r(1) - 3.0) > 0.001) error stop
  if (abs(r(2) - 3.0) > 0.001) error stop
  print *, "PASSED"
end program
