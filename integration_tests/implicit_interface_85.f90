! A call through a procedure pointer in a DO WHILE condition calls the
! procedure the pointer is associated with each time the condition is
! evaluated, also after CYCLE.
program implicit_interface_85
  implicit none
  procedure(real), pointer :: p
  real, external :: ii85_g1, ii85_g2
  integer :: it
  real :: x
  p => ii85_g1
  it = 0
  x = 1.0
  do while (p(x) > 0.0)
    it = it + 1
    p => ii85_g2
    if (it > 5) exit
  end do
  if (it /= 1) error stop 1
  p => ii85_g1
  it = 0
  outer: do while (p(1.0) > 0.0)
    it = it + 1
    p => ii85_g2
    if (it == 1) cycle outer
    error stop 2
  end do outer
  if (it /= 1) error stop 3
  p => ii85_g1
  it = 0
  do while (p(1.0) > 0.0)
    do while (p(2.0) > 0.0)
      p => ii85_g2
    end do
    it = it + 1
  end do
  if (it /= 1) error stop 4
  it = 0
  do while (ii85_g1(real(it)) < 3.0)
    it = it + 1
  end do
  if (it /= 3) error stop 5
  print *, "ok"
end program

real function ii85_g1(x)
  real :: x
  ii85_g1 = x
end function

real function ii85_g2(x)
  real :: x
  ii85_g2 = -x
end function
