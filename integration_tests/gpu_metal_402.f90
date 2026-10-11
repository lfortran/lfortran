module gpu_metal_402_m
  implicit none
contains
  pure subroutine total(k, s)
    integer, intent(in) :: k
    real, intent(out) :: s
    s = 1 + k
  end subroutine
  pure subroutine twice(n)
    integer, intent(inout) :: n
    n = 2 * n
  end subroutine
  pure subroutine upd(a, k)
    integer, intent(inout) :: a(:)
    integer, intent(in) :: k
    call twice(a(k))
  end subroutine
end module
program gpu_metal_402
  use gpu_metal_402_m
  implicit none
  real :: s1(3)
  integer :: b(3), c(3), i
  b = [1, 2, 3]
  c = [4, 5, 6]
  do concurrent (i = 1:3)
    call total(i, s1(i))
    call twice(b(i))
    call upd(c, i)
  end do
  print *, s1, b, c
  if (any(s1 /= [2, 3, 4])) error stop 1
  if (any(b /= [2, 4, 6])) error stop 2
  if (any(c /= [8, 10, 12])) error stop 3
end program
