! Logical named constant arrays of every kind, stored in a module and
! read back from it (#15220).
module parameter_26_mod
implicit none
logical, parameter :: v(2) = [.true., .false.]
logical(1), parameter :: v1(3) = [.false., .true., .true.]
logical(2), parameter :: v2(3) = [.true., .true., .false.]
logical(8), parameter :: v8(5) = [.false., .true., .false., .false., .true.]
logical(8), parameter :: w8(2) = logical(v, 8)
end module

program parameter_26
use parameter_26_mod, only: v, v1, v2, v8, w8
implicit none
if (size(v) /= 2 .or. .not. v(1) .or. v(2)) error stop
if (size(v1) /= 3 .or. v1(1) .or. .not. v1(2) .or. .not. v1(3)) error stop
if (size(v2) /= 3 .or. .not. v2(1) .or. .not. v2(2) .or. v2(3)) error stop
if (size(v8) /= 5 .or. count(v8) /= 2 .or. .not. v8(5)) error stop
if (v8(1) .or. v8(3) .or. v8(4) .or. .not. v8(2)) error stop
if (.not. w8(1) .or. w8(2)) error stop
if (kind(w8) /= 8 .or. kind(v2) /= 2) error stop
print *, v, v1, v2, v8, w8
end program
