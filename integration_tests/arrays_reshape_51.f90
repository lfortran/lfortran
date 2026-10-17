program arrays_reshape_51
  ! Associate a name with the reshape of a local allocatable array, which
  ! --fast turns into a fixed-size array.
  implicit none
  integer, allocatable :: a(:)
  integer :: s(2)
  allocate(a(6))
  a = [1, 2, 3, 4, 5, 6]
  associate(b => reshape(a, [size(a), 1]))
    if (size(b, 1) /= 6 .or. size(b, 2) /= 1) error stop
    if (b(1, 1) /= 1 .or. b(4, 1) /= 4 .or. b(6, 1) /= 6) error stop
  end associate
  s = [2, 3]
  associate(c => reshape(a, s))
    if (size(c, 1) /= 2 .or. size(c, 2) /= 3) error stop
    if (c(1, 1) /= 1 .or. c(2, 1) /= 2 .or. c(1, 3) /= 5 .or. c(2, 3) /= 6) error stop
  end associate
  print *, "ok"
end program arrays_reshape_51
