module gpu_metal_433_m
  ! Internal procedures called from a loop read the named constants and the
  ! variables of their host, also through another internal procedure.
  implicit none
  real, parameter :: false = 0., true = 1.
contains
  subroutine s(x, y, z, w, c)
    real, intent(in) :: x(:,:), w(:), c
    real, intent(out) :: y(:), z(:)
    real, parameter :: mid = 0.5
    integer :: i
    do concurrent (i = 1:size(y))
      z(i) = merge(true, false, x(1,i) > mid)
      y(i) = g(x(:,i))
    end do
  contains
    pure function g(v) result(r)
      real, intent(in) :: v(:)
      real :: r
      r = merge(true, false, sum(v) > 0.99) + h(v)
    end function
    pure function h(v) result(r)
      real, intent(in) :: v(:)
      real :: r
      r = sum(v*w) * c + mid
    end function
  end subroutine
end module

program gpu_metal_433
  use gpu_metal_433_m
  implicit none
  real :: x(2,3), y(3), z(3)
  x = reshape([1., 0., 0., 0., 1., 1.], [2, 3])
  call s(x, y, z, [10., 100.], 2.)
  print *, y
  print *, z
  if (any(y /= [21.5, 0.5, 221.5])) error stop
  if (any(z /= [1., 0., 1.])) error stop
end program
