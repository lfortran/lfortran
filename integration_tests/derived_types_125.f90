program derived_types_125
  ! Test: array constructor of derived type with allocatable component
  ! Ensures no double-free when assigning [wrapper([1])] to an allocatable array.
  implicit none
  type :: wrapper
    integer, allocatable :: b(:)
  end type
  type(wrapper), allocatable :: arr(:)

  arr = [wrapper([1])]
  if (size(arr) /= 1) error stop
  if (size(arr(1)%b) /= 1) error stop
  if (arr(1)%b(1) /= 1) error stop

  arr = [wrapper([10, 20, 30])]
  if (size(arr) /= 1) error stop
  if (size(arr(1)%b) /= 3) error stop
  if (arr(1)%b(1) /= 10) error stop
  if (arr(1)%b(2) /= 20) error stop
  if (arr(1)%b(3) /= 30) error stop

  print *, "ok"
end program
