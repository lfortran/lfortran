program reduce_03
    integer :: i
    integer :: x
    x = 0

    do concurrent (i=1:4) reduce(ior:x)
        x = ior(x, i)
    end do

    if (x /= 7) stop 1
end program
