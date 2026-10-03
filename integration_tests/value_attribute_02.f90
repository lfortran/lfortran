module value_attribute_02_mod
contains
  subroutine scale_arr(arr)
    real(8), value :: arr(3)
    arr = arr * 2.0d0
    if (abs(arr(1) - 2.0d0) > 1.0d-12) error stop "FAIL: arr(1) inside"
    if (abs(arr(2) - 4.0d0) > 1.0d-12) error stop "FAIL: arr(2) inside"
    if (abs(arr(3) - 6.0d0) > 1.0d-12) error stop "FAIL: arr(3) inside"
  end subroutine

  subroutine bump_int(arr)
    integer, value :: arr(4)
    integer :: i
    do i = 1, 4
      arr(i) = arr(i) + 10
    end do
    if (arr(1) /= 11) error stop "FAIL: int arr(1) inside"
    if (arr(4) /= 14) error stop "FAIL: int arr(4) inside"
  end subroutine
end module

program value_attribute_02
  use value_attribute_02_mod
  real(8) :: a(3)
  integer :: b(4)

  a = [1.0d0, 2.0d0, 3.0d0]
  call scale_arr(a)
  if (abs(a(1) - 1.0d0) > 1.0d-12) error stop "FAIL: a(1) modified outside"
  if (abs(a(2) - 2.0d0) > 1.0d-12) error stop "FAIL: a(2) modified outside"
  if (abs(a(3) - 3.0d0) > 1.0d-12) error stop "FAIL: a(3) modified outside"

  b = [1, 2, 3, 4]
  call bump_int(b)
  if (b(1) /= 1) error stop "FAIL: b(1) modified outside"
  if (b(4) /= 4) error stop "FAIL: b(4) modified outside"

  print *, "PASS"
end program
