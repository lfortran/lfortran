module gpu_metal_465_m
  ! A kernel launched in an internal procedure writes a variable of its
  ! host procedure, which another internal procedure then reads by host
  ! association. The launch does not wait for the kernel, so the host has
  ! to wait before the call that reads the variable, although the call
  ! hands it nothing.
  implicit none
contains
  subroutine outer(res)
    real, intent(out) :: res
    real :: x(10)
    integer :: k
    x = 0
    res = 0
    do k = 1, 3
      call add_one()
    end do
  contains
    subroutine add_one()
      integer :: i
      do concurrent (i = 1:10)
        x(i) = x(i) + 1
      end do
      call accumulate()
    end subroutine
    subroutine accumulate()
      res = res + sum(x)
    end subroutine
  end subroutine
end module

program gpu_metal_465
  use gpu_metal_465_m
  implicit none
  real :: r
  call outer(r)
  if (abs(r - 60) > 1e-3) error stop 1
  print *, "ok"
end program
