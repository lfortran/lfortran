module gpu_metal_393_m
  ! A device routine with an allocatable dummy array that it only reads.
  ! The kernel receives the array as a plain one, which is all the dummy
  ! needs, so the routine takes it as a plain array, which keeps the lower
  ! bounds of the actual argument.
  implicit none
contains
  pure function doubled(v, k) result(r)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: k
    real :: r
    r = 2*v(k) + size(v)
  end function

  pure function shifted(v, k) result(r)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: k
    real :: r
    r = v(k) + 100*lbound(v, 1) + 1000*ubound(v, 1)
  end function
end module

program gpu_metal_393
  use gpu_metal_393_m
  implicit none
  real, allocatable :: v(:), w(:)
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
  allocate(w(-2:5))
  w = [(real(i), i = -2, 5)]
  do concurrent (i = 1:8)
    y(i) = shifted(w, i - 3)
  end do
  do i = 1, 8
    if (abs(y(i) - (i - 3 - 200 + 5000)) > 1e-3) error stop 2
  end do
  print *, "ok"
end program
