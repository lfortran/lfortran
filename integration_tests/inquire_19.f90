program inquire_19
    ! INQUIRE(POS=) must return positions beyond 2 GiB in an integer(8)
    ! variable without truncating them to 32 bits.
    use iso_c_binding, only: c_int, c_int64_t
    implicit none
    interface
        integer(c_int) function create_inquire_19_file(length) bind(c)
            import c_int, c_int64_t
            integer(c_int64_t), value :: length
        end function
    end interface
    character(*), parameter :: filename = "inquire_19.dat"
    integer(c_int64_t), parameter :: length = 5000000000_c_int64_t
    integer :: u, stat, p4
    integer(2) :: p2
    integer(8) :: p8, pa(2)
    character :: byte

    ! A sparse file keeps this fast and small on every platform.
    if (create_inquire_19_file(length) /= 0) error stop 1
    open(newunit=u, file=filename, access="stream", form="unformatted", &
        status="old", action="readwrite")

    write(u, pos=3000000001_8) 1_4
    inquire(unit=u, pos=p8)
    if (p8 /= 3000000005_8) then
        print *, "UNIT pos:", p8
        error stop 2
    end if
    inquire(file=filename, pos=p8)
    if (p8 /= 3000000005_8) error stop 3

    read(u, pos=2147483648_8, iostat=stat) byte
    if (stat /= 0) error stop 4
    inquire(unit=u, pos=pa(2))
    if (pa(2) /= 2147483649_8) error stop 5

    read(u, pos=4294967296_8, iostat=stat) byte
    if (stat /= 0) error stop 6
    inquire(unit=u, pos=p8)
    if (p8 /= 4294967297_8) error stop 7

    read(u, pos=10, iostat=stat) byte
    if (stat /= 0) error stop 8
    inquire(unit=u, pos=p4)
    if (p4 /= 11) error stop 9
    inquire(unit=u, pos=p2)
    if (p2 /= 11_2) error stop 10
    inquire(file=filename, pos=p4)
    if (p4 /= 11) error stop 11

    close(u, status="delete")
    print *, "ok"
end program
