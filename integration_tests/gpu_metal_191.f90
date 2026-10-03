module gpu_metal_191_m
  implicit none
  type :: t
    real, allocatable :: v(:)
  end type
contains
  pure function vals(self) result(r)
    type(t), intent(in) :: self
    real, allocatable :: r(:)
    r = self%v
  end function
end module

program gpu_metal_191
  use gpu_metal_191_m
  implicit none
  integer, parameter :: n = 4
  type(t) :: a(n)
  real :: b(n)
  integer :: i

  do i = 1, n
    allocate(a(i)%v(2))
    a(i)%v = [1.0, 2.0]
  end do

  do concurrent(i = 1:n)
    b(i) = compute(a(i))
  end do

  do i = 1, n
    if (abs(b(i) - 3.0) > 1.0e-6) error stop
  end do
  print *, "ok"
contains
  pure function compute(x) result(y)
    type(t), intent(in) :: x
    real :: y
    y = sum(vals(x))
  end function
end program
