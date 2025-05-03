program read_from_file
    implicit none
    character(len=20) :: s 
    integer :: ios
    character(len=100) ::sa

    open(unit=10, file="iostat_test.txt", status="old", iostat=ios)
    if (ios /= 0) then
        print *, "Error opening the file."
        stop
    end if

    print * , ios
    read(10, *, iostat=ios) s
    print * , ios



    print *, trim(s)

    close(10)

end program read_from_file