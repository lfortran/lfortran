program bindc_58
    use bindc_58_module, only: nc_inq
    use iso_c_binding, only: c_char
    implicit none
    character(kind=c_char) :: memfile(4)
    integer :: ios

    open(7, file="bindc_58_scratch.dat", form="unformatted", access="stream", &
         status="replace")
    write(7) "ABCD"
    close(7)

    open(7, file="bindc_58_scratch.dat", form="unformatted", access="stream", &
         status="old")
    read(7, iostat=ios) memfile(1:4)
    close(7, status="delete")

    print *, memfile
    if (ios /= 0) error stop
    if (memfile(1) /= "A") error stop
    if (memfile(2) /= "B") error stop
    if (memfile(3) /= "C") error stop
    if (memfile(4) /= "D") error stop
end program
