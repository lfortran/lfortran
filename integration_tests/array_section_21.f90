program array_section_21
  implicit none
  ! Use n = 200000 to trigger segfault without the fix
  integer, parameter :: n = 100
  real, allocatable :: a(:,:)
  real :: s
  integer :: i
  allocate(a(1,n))
  a = 1.0
  s = 0.0
  do i = 1, n
    call use_arr(a(:,i), s)
  end do
  if (nint(s) /= n) error stop
  print *, "ok"
contains
  subroutine use_arr(x, s)
    real, intent(in) :: x(:)
    real, intent(inout) :: s
    s = s + x(1)
  end subroutine
end program
