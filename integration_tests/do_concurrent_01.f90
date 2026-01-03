program do_concurrent_01
  integer :: i, n
  real :: x(12)
  n = 12

  x = 0.49

  do concurrent(i = 1:n)
    print *, x(i)
    if (abs(x(i) - 0.49) > 1e-6) error stop
  end do
end program
