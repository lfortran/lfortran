program logical_kind_07
  implicit none
  logical(8) :: x8

  x8 = .true.

  ! Without KIND: must return default logical kind
  if (kind(logical(x8)) /= kind(.true.)) error stop

  ! With explicit KIND: must use the specified kind
  if (kind(logical(x8, kind=8)) /= 8) error stop
  if (kind(logical(x8, kind=4)) /= 4) error stop

end program
