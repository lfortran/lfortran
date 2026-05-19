program reduce_01
    integer :: i
    integer :: x
    x = 0

    do concurrent (i=1:4) reduce(ieor:x)
        x = ieor(x, i)
    end do

    if (x /= 4) stop 1
end program
