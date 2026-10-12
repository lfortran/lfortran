module gpu_metal_401_m
  implicit none
contains
  pure subroutine total(k, s)
    integer, intent(in) :: k
    real, intent(out) :: s
    s = 1 + k
  end subroutine

  pure subroutine scale(x, y)
    real, intent(in) :: x
    real, intent(inout) :: y
    y = y * x
  end subroutine
end module

program gpu_metal_401
  use gpu_metal_401_m
  implicit none
  real :: s1(3)
  integer :: i
  do concurrent (i = 1:3)
    block
      real :: t
      call total(i, t)
      call scale(real(i), t)
      s1(i) = t
    end block
  end do
  print *, s1
  if (any(s1 /= [2, 6, 12])) error stop
end program
