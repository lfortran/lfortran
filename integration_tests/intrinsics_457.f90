program intrinsics_457
implicit none
integer, parameter :: nrow = 3, ncol = 4
integer :: b(nrow, ncol), r2(nrow, ncol), r3(nrow, ncol)
integer :: expected1(nrow, ncol), expected2(nrow, ncol)
integer :: i, j

do j = 1, ncol
    do i = 1, nrow
        b(i, j) = i * 10 + j
    end do
end do

! eoshift along dim=1: each column is shifted by 1, last row filled with 0
r2 = eoshift(b, shift=1, dim=1)
do j = 1, ncol
    expected1(1, j) = 20 + j
    expected1(2, j) = 30 + j
    expected1(3, j) = 0
end do
if (any(r2 /= expected1)) error stop

! eoshift along dim=2 with negative shift and explicit boundary
r3 = eoshift(b, shift=-1, boundary=99, dim=2)
do i = 1, nrow
    expected2(i, 1) = 99
    expected2(i, 2) = i * 10 + 1
    expected2(i, 3) = i * 10 + 2
    expected2(i, 4) = i * 10 + 3
end do
if (any(r3 /= expected2)) error stop

print *, "ok"
end program
