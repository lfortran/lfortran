program parameter_23
! Local named constant arrays are read through their elements
implicit none
integer, parameter :: ip(3) = [1, -2, 3]
integer(8), parameter :: ip8(2) = [10000000000_8, -3_8]
real, parameter :: rp(3) = [0.5, 1.5, -2.5]
real(8), parameter :: rp8(2) = [1.5d0, -2.25d0]
logical, parameter :: lp(3) = [.true., .false., .true.]
complex, parameter :: cp(2) = [(1.0, 2.0), (-3.0, 0.5)]
integer :: i3(3)
integer(8) :: i8(2)
real :: r3(3)
real(8) :: r8(2)
logical :: l3(3)
complex :: c2(2)
i3 = ip
i8 = ip8
r3 = rp
r8 = rp8
l3 = lp
c2 = cp
if (i3(1) /= 1 .or. i3(2) /= -2 .or. i3(3) /= 3) error stop 1
if (i8(1) /= 10000000000_8 .or. i8(2) /= -3_8) error stop 2
if (r3(1) /= 0.5 .or. r3(2) /= 1.5 .or. r3(3) /= -2.5) error stop 3
if (r8(1) /= 1.5d0 .or. r8(2) /= -2.25d0) error stop 4
if (.not. l3(1) .or. l3(2) .or. .not. l3(3)) error stop 5
if (c2(1) /= (1.0, 2.0) .or. c2(2) /= (-3.0, 0.5)) error stop 6
print *, "ok"
end program
