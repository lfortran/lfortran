program reduce_02
    integer :: i
    integer :: x
    x = -1

    do concurrent (i=1:4) reduce(iand:x)
        x = iand(x, i)
    end do

    if (x /= 0) stop 1
end program
