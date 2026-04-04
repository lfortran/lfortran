module gpu_metal_83_m
  implicit none
  type :: t
    integer, allocatable :: x(:)
  contains
    procedure :: n
  end type
contains
  pure integer function n(self)
    class(t), intent(in) :: self
    n = size(self%x)
  end function
end module

program gpu_metal_83
! Test: size() on allocatable derived-type member via type-bound procedure
! inside do concurrent (Metal GPU offload)
  use gpu_metal_83_m
  implicit none
  type(t) :: obj
  integer :: r(1)
  integer :: i
  allocate(obj%x(3))
  obj%x = [10, 20, 30]
  do concurrent (i = 1:1)
    r(i) = obj%n()
  end do
  if (r(1) /= 3) error stop
  print *, r(1)
end program

