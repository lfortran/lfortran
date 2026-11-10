module spec_expr_12_mod
  implicit none
contains

  pure function doubler(dim_1) result (length)
    integer, intent(in) :: dim_1
    integer :: length
    length = dim_1 * 2
  end function

  ! the result extent is a pure function of a local named constant
  function ff() result(arr)
    integer, parameter :: dim_1 = 10
    integer :: arr(doubler(dim_1))
    integer :: i
    do i = 1, size(arr)
      arr(i) = i
    end do
  end function

end module spec_expr_12_mod

program spec_expr_12
  use spec_expr_12_mod
  implicit none
  integer :: r(20)
  print *, size(ff())
  if (size(ff()) /= 20) error stop
  r = ff()
  if (size(r) /= 20) error stop
  if (r(20) /= 20) error stop
  if (sum(ff()) /= 210) error stop
end program spec_expr_12
