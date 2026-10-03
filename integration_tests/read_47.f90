program read_47
    implicit none
    character(256) :: s
    character(60) :: val1 = "''"
    character(60) :: val2 = "'hello'"
    character(60) :: val3 = "'it''s'"
    character(60) :: val4 = '"world"'

    read(val1, *) s
    if (len_trim(s) /= 0) error stop 1

    read(val2, *) s
    if (trim(s) /= "hello") error stop 2

    read(val3, *) s
    if (trim(s) /= "it's") error stop 3

    read(val4, *) s
    if (trim(s) /= "world") error stop 4

end program read_47
