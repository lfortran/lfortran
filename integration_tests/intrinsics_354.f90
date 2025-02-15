program intrinsics_354
  integer :: x(2)
  x = [1, 2]
  call sub(x)
  print *, x
  if (any(x /= [1, 2])) error stop
  contains
  subroutine sub(x)
    integer :: x(2)
    real :: arr(minval(abs(x)))
  end subroutine sub
end program