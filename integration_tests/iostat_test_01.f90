program read_from_file
    implicit none
    character(len=20) :: s 
    integer :: ios

    open(unit=10, file="iostat_test_01..txt", status="old", iostat=ios)
    if (ios /= 0) then
        print *, "Error opening the file."
        stop
    end if

    if(ios /= 0) error stop
    read(10,*,iostat=ios) s
    if(ios == 0) error stop

    read(10,*,iostat=ios) s
    if(ios /= 0) error stop


    close(10)
end program read_from_file
