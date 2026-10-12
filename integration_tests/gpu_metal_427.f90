module gpu_metal_427_m
  ! With --fast, a function called inside an expression is inlined, which
  ! binds an array dummy to its actual as a pointer: the dummy then has the
  ! bounds of the actual. The dummies here declare lower bounds of their
  ! own -- one for an assumed-shape dummy, and, for an allocatable dummy,
  ! those device code hands it as dummies of their own -- and the actual
  ! does not start at one.
  implicit none
contains
  pure function entry(a, k) result(r)
    integer, intent(in) :: a(:,:)
    integer, intent(in) :: k
    integer :: r
    r = a(4, k) + 10*lbound(a, 1)
  end function
  pure function entry2(a, k) result(r)
    integer, allocatable, intent(in) :: a(:,:)
    integer, intent(in) :: k
    integer :: r
    r = a(2, k) + 10*lbound(a, 1)
  end function
end module

program gpu_metal_427
  use gpu_metal_427_m
  implicit none
  integer, allocatable :: y(:,:)
  integer :: i, r4(3), r5(3)
  allocate(y(-1:2, 0:2))
  y = 0
  y(2, :) = [4, 5, 6]
  do concurrent (i = 1:3)
    r4(i) = entry(y, i)
    r5(i) = entry2(y, i - 1)
  end do
  print *, r4, r5
  if (any(r4 /= [14, 15, 16])) error stop 1
  if (any(r5 /= [-6, -5, -4])) error stop 2
end program
