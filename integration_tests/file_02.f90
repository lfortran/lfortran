program file_02
    implicit none
    integer :: iostat, unit=10, i
    open(unit, file="../file_01_data.txt")

    read(unit, *, iostat=iostat) i
    if (is_iostat_end(iostat)) then
        print *, "End of file reached."
        stop
    end if
    print *, i
    close(unit)
end program file_02
