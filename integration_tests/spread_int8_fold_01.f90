program spread_int8_fold_01
! spread of an integer(8) constant array keeps all 64 bits of each element
! when folded in a PARAMETER initializer (#17874)
implicit none
integer(8), parameter :: h = huge(1_8)
integer(8), parameter :: a(3) = [h, -h, 4294967296_8]
integer(8), parameter :: s1(2,3) = spread(a, 1, 2)
integer(8), parameter :: s2(3,2) = spread(a, 2, 2)
integer :: i

do i = 1, 2
    if (s1(i,1) /= h) error stop 1
    if (s1(i,2) /= -h) error stop 2
    if (s1(i,3) /= 4294967296_8) error stop 3
    if (s2(1,i) /= h) error stop 4
    if (s2(2,i) /= -h) error stop 5
    if (s2(3,i) /= 4294967296_8) error stop 6
end do
print *, "ok"
end program
