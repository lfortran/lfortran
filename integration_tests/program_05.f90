 1  &
    integer :: i
    i = 0
 2  i = i + 1
    if (i < 3) goto 2
    if (i /= 3) error stop
    print *, i
end program
