program findloc_int8_parameter_01
! findloc of integer(8) constant arrays folds exactly, also for values that
! differ only beyond 2**53 (not representable in double precision)
implicit none
integer(8), parameter :: big = 9007199254740992_8
integer(8), parameter :: c(3) = [big + 1, big, big + 1]
integer, parameter :: f1(1) = findloc(c, big)
integer, parameter :: f2(1) = findloc(c, big + 1, back=.true.)
integer, parameter :: f3 = findloc(c, big, dim=1)
integer, parameter :: f4(1) = findloc(c, big + 1, mask=[.false., .true., .true.])
integer(8) :: v(3)
if (f1(1) /= 2) error stop 1
if (f2(1) /= 3) error stop 2
if (f3 /= 2) error stop 3
if (f4(1) /= 3) error stop 4
if (any(findloc([big + 1, big], big) /= 2)) error stop 5
v = c
if (any(findloc(v, big) /= 2)) error stop 6
print *, "ok"
end program
