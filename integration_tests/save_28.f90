! Bare SAVE must not turn a named constant (PARAMETER) into a non-constant.
! Pattern from reference LAPACK zlacon.f (#15975)
program save_28
implicit none
complex(8) :: x(4)
integer :: i
call fill_cm(4, x)
do i = 1, 4
  if (abs(real(x(i)) - 0.25d0) > 1d-12) error stop
  if (abs(aimag(x(i))) > 1d-12) error stop
end do
print *, "ok"
contains
subroutine fill_cm(n, x)
integer :: n, i
complex(8) :: x(n)
integer, parameter :: dp = kind(0.0d0)
save
do i = 1, n
  x(i) = cmplx(1.0d0 / dble(n), kind=dp)
end do
end subroutine
end program
