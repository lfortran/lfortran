program allocate_69
  implicit none
  integer, allocatable :: r(:)
  r = make()
  ! Intrinsic assignment to an unallocated allocatable LHS must allocate it
  ! with lower bound 1 in every dimension, regardless of the bounds of the
  ! function-result allocatable RHS.
  if (lbound(r,1) /= 1) error stop "lb"
  if (ubound(r,1) /= 5) error stop "ub"
  if (size(r) /= 5) error stop "size"
  if (r(1) /= 100) error stop "v1"
  if (r(5) /= 500) error stop "v5"
  print *, "PASS"
contains
  function make() result(out)
    integer, allocatable :: out(:)
    integer :: i
    allocate(out(-2:2))
    do i = -2, 2
       out(i) = (i+3)*100
    end do
  end function
end program
