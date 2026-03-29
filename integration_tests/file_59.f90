program file_59
    implicit none
    character(len=20) :: enc

    ! Open a file with a specific encoding
    open(unit=10, file="file_59_file.txt", encoding="UTF-8", form="formatted")

    ! Inquire the encoding of the file
    inquire(unit=10, encoding=enc)

    print *, "Encoding of file:", trim(enc)
    if (trim(enc) /= "UTF-8") error stop

    close(10)
    open(unit=10, file="file_59_file.txt",  form="formatted")

    ! Inquire the encoding of the file
    inquire(unit=10, encoding=enc)

    print *, "Encoding of file:", trim(enc)
    if (trim(enc) /= "UNKNOWN") error stop
    
    close(10)
end program file_59