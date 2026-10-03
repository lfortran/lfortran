program gpu_metal_64
implicit none
type :: t
  real :: s = 2.0
end type
type(t) :: obj
real, allocatable :: x(:)
allocate(x(4))
x = 1.0
call compute()
if (abs(x(1) - 2.0) > 1e-5) error stop
if (abs(x(2) - 2.0) > 1e-5) error stop
if (abs(x(3) - 2.0) > 1e-5) error stop
if (abs(x(4) - 2.0) > 1e-5) error stop
print *, "PASSED"
contains
  subroutine compute()
    integer :: i
    do concurrent(i = 1:4)
      x(i) = x(i) * obj%s
    end do
  end subroutine
end program
