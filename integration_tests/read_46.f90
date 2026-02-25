! This program tests various compilation of various kinds
! for iostat variable
program read_46
    implicit none

    character(len=10) :: str
    integer :: i1
    integer(kind = 1):: istat1
    integer(kind = 2):: istat2
    integer(kind = 4):: istat3
    integer(kind = 8):: istat4

    str = "111"
    i1 = 1
    read(str, *, iostat=istat1) i1  
    print*, i1, istat1
    if (i1 /= 111) error stop
    if (istat1 /= 0) error stop

    i1 = 2 
    str = "422"
    read(str, *, iostat=istat2) i1  
    print*, i1, istat2
    if (i1 /= 422) error stop
    if (istat2 /= 0) error stop

    i1 = 3
    str = "1234"
    read(str, *, iostat=istat3) i1  
    print*, i1, istat3
    if (i1 /= 1234) error stop
    if (istat3 /= 0) error stop

    str = "45"
    i1 = 1
    read(str, *, iostat=istat4) i1  
    print*, i1, istat1
    if (i1 /= 45) error stop
    if (istat1 /= 0) error stop

end program read_46