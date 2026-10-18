program read_unconnected_unit_01
    implicit none
    integer :: k, ios
    namelist /nl/ k

    write(81, *) 42
    close(81)
    k = 0
    read(81, *) k
    if (k /= 42) error stop
    close(81, status='delete')

    write(82) 7
    close(82)
    k = 0
    read(82) k
    if (k /= 7) error stop
    close(82, status='delete')

    write(83, '(a)') '&nl k = 5 /'
    close(83)
    k = 0
    read(83, nml=nl)
    if (k /= 5) error stop
    close(83, status='delete')

    read(84, *, iostat=ios) k
    if (ios >= 0) error stop
    close(84, status='delete')

    print *, 'ok'
end program
