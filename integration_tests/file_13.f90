program file_13
    implicit none

    integer :: num

    open(UNIT=1, file="file_01_data.txt", form="formatted", access="stream", status="old")
    read(1, *) num
    close(1)

    print *, num
    if (num /= 10130) error stop

end program
