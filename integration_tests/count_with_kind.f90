program count_with_kind
  integer, parameter :: k = kind(0)
  logical :: a(4, 5)

  a = .true.

  print *, count(a, 2, kind=k)

  if (any(count(a, 2, kind=k) /= 5)) stop 1
end program