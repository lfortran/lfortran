program inquire_18
    ! INQUIRE without POS= on a unit that is being written to, and
    ! INQUIRE with POS= (default and non-default kind) on a stream unit
    implicit none
    integer :: i, u, v, p
    integer(8) :: p8
    logical :: op

    open(newunit=u, status='scratch', form='formatted')
    do i = 1, 1000
        inquire(unit=u, opened=op)
        if (.not. op) error stop 1
        write(u, '(i0)') i
    end do
    rewind(u)
    do i = 1, 1000
        read(u, *) p
        if (p /= i) error stop 2
    end do
    close(u)

    open(newunit=v, status='scratch', access='stream', form='formatted')
    inquire(unit=v, pos=p)
    if (p /= 1) error stop 3
    write(v, '(a)') 'abc'
    inquire(unit=v, opened=op)
    if (.not. op) error stop 4
    inquire(unit=v, pos=p)
    if (p /= 5) error stop 5
    write(v, '(a)') 'de'
    inquire(unit=v, pos=p8)
    if (p8 /= 8_8) error stop 6
    close(v)
    print *, "ok"
end program
