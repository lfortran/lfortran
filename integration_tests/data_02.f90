program data2
    implicit none
    integer :: i, iarx(3,1), iary(3,1)
    print *, "1"
    if (.true.) then
        print *, "2"
        data(iarx(i,1), iary(i,1),i=1,3)/  1, 9, 1950,1350, 4350, 4/
    else
        data(iarx(i,1), iary(i,1),i=1,3)/  1, 9, 1950,1350, 4350, 4/
        print *, "3"
    end if
    print *, "4"
    data(iary(i,1),i=1,3)/  1, 9, 1950 /
    print *, "5"
end program
