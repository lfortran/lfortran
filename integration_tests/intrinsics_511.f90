! ANY(ISNAN(...)) applied to a component of a rank-2 array of derived-type
! instances (#11640).
module intrinsics_511_mod
  implicit none

  type :: point
     real(8) :: x(3)
  end type

  type(point), allocatable :: top(:, :)
end module intrinsics_511_mod

program intrinsics_511
  use intrinsics_511_mod
  implicit none
  integer :: i, j
  real(8) :: zero

  allocate(top(2, 2))
  do j = 1, 2
     do i = 1, 2
        top(i, j)%x = [1.0d0, 2.0d0, 3.0d0]
     end do
  end do

  if (any(isnan(top(:, :)%x(3)))) then
     error stop
  else
     print *, 'ok'
  end if
  if (count(isnan(top(:, :)%x(3))) /= 0) error stop

  zero = 0.0d0
  top(2, 1)%x(3) = zero / zero
  if (.not. any(isnan(top(:, :)%x(3)))) error stop
  if (count(isnan(top(:, :)%x(3))) /= 1) error stop
  if (any(isnan(top(:, :)%x(2)))) error stop
end program intrinsics_511
