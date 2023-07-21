program file_02
    implicit none
    integer :: iostat, unit=10, i
    open(unit, file="file_01_data.txt")

    ! TODO: return iostat value
    read(unit, *, iostat=iostat) i
    if (is_iostat_end(iostat)) then
        print *, "End of file reached."
        stop
    else if (is_iostat_eor(iostat)) then
        print *, "End of record reached."
        stop
    end if
    print *, i
    if (i /= 10130) error stop
    close(unit)
end program file_02
