program unpack_parameter_01
! unpack of integer(1), integer(2) and character constants in a parameter
implicit none
logical, parameter :: m(4) = [.true., .false., .false., .true.]
integer(1), parameter :: a1(2) = [-huge(1_1) - 1_1, 127_1]
integer(2), parameter :: a2(2) = [-huge(1_2) - 1_2, huge(1_2)]
character(2), parameter :: ac(2) = ['ab', 'cd']
integer(1), parameter :: u1(4) = unpack(a1, m, 5_1)
integer(1), parameter :: v1(4) = unpack(a1, m, [1_1, 2_1, -3_1, 4_1])
integer(2), parameter :: u2(4) = unpack(a2, m, -7_2)
integer(2), parameter :: v2(4) = unpack(a2, m, [1_2, 2_2, 3_2, 4_2])
character(2), parameter :: uc(4) = unpack(ac, m, 'zz')
character(2), parameter :: vc(4) = unpack(ac, m, ['w1', 'x2', 'y3', 'z4'])

if (kind(u1) /= 1 .or. kind(u2) /= 2) error stop
if (any(u1 /= [-huge(1_1) - 1_1, 5_1, 5_1, 127_1])) error stop
if (any(v1 /= [-huge(1_1) - 1_1, 2_1, -3_1, 127_1])) error stop
if (any(u2 /= [-huge(1_2) - 1_2, -7_2, -7_2, huge(1_2)])) error stop
if (any(v2 /= [-huge(1_2) - 1_2, 2_2, 3_2, huge(1_2)])) error stop
if (any(uc /= ['ab', 'zz', 'zz', 'cd'])) error stop
if (any(vc /= ['ab', 'x2', 'y3', 'cd'])) error stop
print *, u1, v1
print *, u2, v2
print *, uc, vc
end program
