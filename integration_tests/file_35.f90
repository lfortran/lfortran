program file_35
    implicit none
    integer :: lun, ios = 8
    character(len=100) :: message

    open(newunit=lun, file="test.txt", status="replace")

    close(unit=lun, iostat=ios, iomsg=message)
    print *, ios
    if (ios /= 0) then
        print *, "Close error:", trim(message)
        error stop
    else
        print *, "File closed successfully."
    end if

end program file_35