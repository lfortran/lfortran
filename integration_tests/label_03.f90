program label_03
! A labeled END IF is a branch target (from within its IF construct)
implicit none
integer :: i, n
n = 0
do i = 1, 4
    if (i > 0) then
        if (i == 2) go to 10
        n = n + i
10  end if
end do
if (n /= 8) error stop
n = 0
do i = 1, 4
    if (i > 2) then
        n = n + 100
    else
        if (i == 1) go to 20
        n = n + i
20  endif
end do
if (n /= 202) error stop
print *, n
end program
