module gpu_metal_387_m
  ! A device routine with an explicit-shape dummy argument whose extent is
  ! another dummy, `a(n)`, called with an array of the thread's own memory.
  ! The call needs a copy of the routine for that memory space, and the copy
  ! of `a` has to read the copy of `n`, not the original routine's.
  implicit none
contains
  pure subroutine fill_sum(n, x, a, r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, intent(inout) :: a(n)
    real, intent(out) :: r
    integer :: i
    do i = 1, n
      a(i) = x*i
    end do
    r = sum(a)
  end subroutine
end module

program gpu_metal_387
  use gpu_metal_387_m
  implicit none
  real :: x(8), y(8)
  integer :: i
  x = [(real(i), i = 1, 8)]
  do concurrent (i = 1:8)
    block
      real :: w(3)
      call fill_sum(3, x(i), w, y(i))
    end block
  end do
  do i = 1, 8
    if (abs(y(i) - 6*x(i)) > 1e-5) error stop 1
  end do
  print *, "ok"
end program
