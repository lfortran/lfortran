program doloop_25
implicit none
integer :: i, j
do i = 1, 5
    block
        if (i == 2) exit
    end block
end do
print *, i
if (i /= 2) error stop
outer: do i = 1, 5
    do j = 1, 3
        if (i == 3) exit outer
    end do
end do outer
print *, i, j
if (i /= 3) error stop
if (j /= 1) error stop
end program
