program data2
    implicit none
    integer :: j
    data j / 5 /
    integer :: i, iarx(3,1), iary(3,1)
    print *, "1", j
    if (.true.) then
        print *, "2"
        data(iarx(i,1), iary(i,1),i=1,3)/  1, 9, 1950,1350, 4350, 4/
        print *, iarx(1, 1)
        if (iarx(1, 1) /= 1) error stop
        print *, iary(1, 1)
        if (iary(1, 1) /= 9) error stop
        print *, iarx(2, 1)
        if (iarx(2, 1) /= 1950) error stop
        print *, iary(2, 1)
        if (iary(2, 1) /= 1350) error stop
        print *, iarx(3, 1)
        if (iarx(3, 1) /= 4350) error stop
        print *, iary(3, 1)
        if (iary(3, 1) /= 4) error stop
    else
        data(iarx(i,1), iary(i,1),i=1,3)/  1, 9, 1950,1350, 4350, 4/
        print *, "3"
    end if
    print *, "4"
    data(iary(i,1),i=1,3)/  1, 9, 1950 /
    print *, "5"
end program
