module value_attribute_03_mod
contains
  subroutine sub(a, n)
    integer, value :: n
    real(8), value :: a(n)
    integer :: i
    if (size(a) /= n) error stop "sz"
    do i = 1, n
       if (abs(a(i) - real(i,8)) > 1.0d-12) error stop "val"
    end do
    a(1) = -999.0d0
    if (abs(a(1) + 999.0d0) > 1.0d-12) error stop "local"
  end subroutine

  subroutine sub2(b, m, k)
    integer, value :: m
    integer, value :: k
    integer, value :: b(m, k)
    integer :: i, j
    if (size(b) /= m*k) error stop "sz2"
    do j = 1, k
       do i = 1, m
          if (b(i, j) /= 10*i + j) error stop "val2"
       end do
    end do
    b(1, 1) = -1
  end subroutine
end module

program value_attribute_03
  use value_attribute_03_mod
  real(8) :: x(5)
  integer :: y(3, 4)
  integer :: i, j
  do i = 1, 5
     x(i) = real(i, 8)
  end do
  call sub(x, 5)
  if (abs(x(1) - 1.0d0) > 1.0d-12) error stop "modified1"
  do j = 1, 4
     do i = 1, 3
        y(i, j) = 10*i + j
     end do
  end do
  call sub2(y, 3, 4)
  if (y(1, 1) /= 11) error stop "modified2"
  print *, "PASS"
end program
