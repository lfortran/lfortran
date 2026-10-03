! Test sequence association: passing an array element to an
! explicit-shape array dummy argument (F2018 15.5.2.11).
subroutine sum_n(n, x, s)
  integer, intent(in) :: n
  integer, intent(in) :: x(n)
  integer, intent(out) :: s
  integer :: i
  s = 0
  do i = 1, n
    s = s + x(i)
  end do
end subroutine

program main
  implicit none
  integer :: work(6), res

  work = [10, 20, 30, 40, 50, 60]

  ! Pass work(3) — sequence association gives callee elements 30,40,50,60
  call sum_n(4, work(3), res)
  print *, res
  if (res /= 180) error stop

  ! Pass work(1) — full array via sequence association
  call sum_n(6, work(1), res)
  print *, res
  if (res /= 210) error stop

  ! Pass work(5) — last two elements: 50+60
  call sum_n(2, work(5), res)
  print *, res
  if (res /= 110) error stop
end program
