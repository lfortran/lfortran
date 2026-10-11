program arrays_reshape_52
  ! Assign a reshape with a shape known only at run time to a local
  ! allocatable array allocated with a constant shape, under --fast.
  implicit none
  integer, allocatable :: a(:, :), b(:, :)
  integer :: s(2) = [4, 2]
  allocate(a(2, 4))
  allocate(b(s(2), s(1)))
  a = 0
  b = reshape([-14, 3, 0, -2, 19, 1, 20, 21], [2, 4])
  a = reshape(b, s)
  if (size(a, 1) /= 4 .or. size(a, 2) /= 2) error stop
  if (a(1, 1) /= -14 .or. a(4, 1) /= -2 .or. a(1, 2) /= 19 .or. a(4, 2) /= 21) error stop
  print *, "ok"
end program arrays_reshape_52
