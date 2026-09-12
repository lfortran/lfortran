module maxloc_component_mask
  implicit none

  type :: point
     real(8) :: x(2)
  end type

  type(point), allocatable :: top(:)

contains

  subroutine get_segment(r, idx)
    real(8), intent(in) :: r
    integer, intent(out) :: idx
    integer :: loc(1)

    loc = maxloc(top(:)%x(1), top(:)%x(1) < r)
    idx = loc(1)
  end subroutine

end module

program intrinsics_470
  use maxloc_component_mask
  implicit none
  integer :: idx

  allocate(top(3))
  top(1)%x = [0.0d0, 0.0d0]
  top(2)%x = [1.0d0, 0.0d0]
  top(3)%x = [2.0d0, 0.0d0]

  call get_segment(1.5d0, idx)

  if (idx /= 2) error stop

end program intrinsics_470
