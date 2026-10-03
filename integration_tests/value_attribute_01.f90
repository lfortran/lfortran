program value_attribute_01
  implicit none
  integer :: x
  real :: y
  logical :: z

  x = 500
  if (divide_val(x) /= 50) error stop "FAIL: divide_val result"
  if (x /= 500) error stop "FAIL: x was modified by divide_val"

  y = 3.0
  if (abs(scale_val(y) - 6.0) > 1.0e-6) error stop "FAIL: scale_val result"
  if (abs(y - 3.0) > 1.0e-6) error stop "FAIL: y was modified by scale_val"

  z = .true.
  if (flip_val(z) .neqv. .false.) error stop "FAIL: flip_val result"
  if (z .neqv. .true.) error stop "FAIL: z was modified by flip_val"

  call modify_val_sub(x)
  if (x /= 500) error stop "FAIL: x was modified by modify_val_sub"

  print *, "PASS"

contains

  pure function divide_val(num) result(res)
    integer, value :: num
    integer :: res
    num = num / 10
    res = num
  end function

  pure function scale_val(val) result(res)
    real, value :: val
    real :: res
    val = val * 2.0
    res = val
  end function

  pure function flip_val(flag) result(res)
    logical, value :: flag
    logical :: res
    flag = .not. flag
    res = flag
  end function

  subroutine modify_val_sub(n)
    integer, value :: n
    n = n + 100
  end subroutine

end program
