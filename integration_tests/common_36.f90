program common_36
implicit none
integer :: arr(5)
integer :: i
do i = 1, 5
    arr(i) = i * 10
end do
call sub(2, arr)
end program

subroutine sub(n, arr)
integer :: n
integer :: icc1
common icc1
integer :: arr(n:icc1)
integer :: i

icc1 = 6
! arr(n:icc1) means arr(2:6), length 5
do i = n, icc1
    if (arr(i) /= (i - n + 1) * 10) error stop
end do
print *, "PASS"
end subroutine
