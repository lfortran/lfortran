module gpu_metal_389_m
  ! A device routine with an allocatable dummy array that it only reads.
  ! The kernel receives the array as a plain one, which is all the dummy
  ! needs, so the routine takes it as a plain array.
  implicit none
contains
  pure function doubled(v, k) result(r)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: k
    real :: r
    r = 2*v(k) + size(v)
  end function
end module

program gpu_metal_389
  use gpu_metal_389_m
  implicit none
  real, allocatable :: v(:)
  real :: y(8)
  integer :: i
  allocate(v(8))
  v = [(real(i), i = 1, 8)]
  do concurrent (i = 1:8)
    y(i) = doubled(v, i)
  end do
  do i = 1, 8
    if (abs(y(i) - (2*i + 8)) > 1e-5) error stop 1
  end do
  print *, "ok"
end program
