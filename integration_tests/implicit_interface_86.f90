! Procedures with implicit interfaces (a subroutine, a function, and a
! procedure pointer) called inside OpenMP parallel regions.
program implicit_interface_86
  implicit none
  integer :: i, total
  real :: a(100), s
  real, external :: ii86_fw
  integer, external :: ii86_twice
  external :: ii86_work
  procedure(), pointer :: pw
  !$omp parallel do
  do i = 1, 100
    call ii86_work(a(i), i)
  end do
  !$omp end parallel do
  if (abs(sum(a) - 5050.0) > 1e-3) error stop 1
  s = 0
  !$omp parallel do reduction(+:s)
  do i = 1, 100
    s = s + ii86_fw(a(i))
  end do
  !$omp end parallel do
  if (abs(s - 10100.0) > 1e-3) error stop 2
  total = 0
  !$omp parallel do reduction(+:total)
  do i = 1, 10
    total = total + ii86_twice(i)
  end do
  !$omp end parallel do
  if (total /= 110) error stop 3
  a = 0
  pw => ii86_work
  !$omp parallel do
  do i = 1, 100
    call pw(a(i), i)
  end do
  !$omp end parallel do
  if (abs(sum(a) - 5050.0) > 1e-3) error stop 4
  print *, s, total
end program

subroutine ii86_work(x, i)
  real :: x
  integer :: i
  x = real(i)
end subroutine

real function ii86_fw(x)
  real :: x
  ii86_fw = 2*x
end function

integer function ii86_twice(i)
  integer :: i
  ii86_twice = 2*i
end function
