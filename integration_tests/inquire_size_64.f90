program inquire_size_64
    use iso_c_binding, only: c_int, c_int64_t
    implicit none
    interface
        integer(c_int) function create_inquire_size_file(length) bind(c)
            import c_int, c_int64_t
            integer(c_int64_t), value :: length
        end function
    end interface
    integer(c_int64_t), parameter :: lengths(6) = [0_c_int64_t, 17_c_int64_t, &
        2147483647_c_int64_t, 2147483648_c_int64_t, 2147483649_c_int64_t, 4294967297_c_int64_t]
    character(*), parameter :: filename = "inquire_size_64.dat"
    integer(c_int64_t) :: length
    integer :: i, unit, small_length, stat, position, number
    logical :: exists, opened
    character(16) :: access_mode, form
    character :: byte

    do i = 1, size(lengths)
        if (create_inquire_size_file(lengths(i)) /= 0) error stop 1
        inquire(file=filename, size=length, exist=exists, opened=opened, iostat=stat)
        if (stat /= 0 .or. .not. exists .or. opened) error stop 2
        if (length /= lengths(i)) then
            print *, "FILE size:", length, "expected:", lengths(i)
            error stop 3
        end if
        open(newunit=unit, file=filename, access="stream", form="unformatted", status="old")
        inquire(unit=unit, size=length, pos=position, opened=opened, number=number, &
            access=access_mode, form=form, iostat=stat)
        if (length /= lengths(i)) error stop 4
        if (stat /= 0 .or. .not. opened .or. number /= unit) error stop 5
        if (position /= 1 .or. access_mode /= "STREAM" .or. form /= "UNFORMATTED") error stop 6
        inquire(file=filename, size=length, opened=opened)
        if (length /= lengths(i) .or. .not. opened) error stop 7
        inquire(unit=unit, pos=position)
        if (position /= 1) error stop 8
        if (lengths(i) <= 2147483647_c_int64_t) then
            inquire(unit=unit, size=small_length)
            if (int(small_length, c_int64_t) /= lengths(i)) error stop 9
            inquire(file=filename, size=small_length)
            if (int(small_length, c_int64_t) /= lengths(i)) error stop 10
        end if
        if (lengths(i) > 1_c_int64_t) then
            read(unit, pos=lengths(i)-1_c_int64_t, iostat=stat) byte
            if (stat /= 0) error stop 15
            inquire(unit=unit, size=length)
            if (length /= lengths(i)) error stop 16
            inquire(file=filename, size=length)
            if (length /= lengths(i)) error stop 17
            read(unit, iostat=stat) byte
            if (stat /= 0) error stop 18
            read(unit, iostat=stat) byte
            if (stat >= 0) error stop 19
        end if
        close(unit, status="delete")
    end do
    inquire(file=filename, size=length, exist=exists)
    if (exists .or. length /= -1_c_int64_t) error stop 11
    inquire(unit=123456, size=length, opened=opened)
    if (opened .or. length /= -1_c_int64_t) error stop 12
    inquire(file=filename, size=small_length)
    if (small_length /= -1) error stop 13
    inquire(unit=123456, size=small_length)
    if (small_length /= -1) error stop 14
end program
