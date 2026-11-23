module file_open_16_user
    ! A user procedure named `newunit` does not change the NEWUNIT=
    ! specifier of OPEN, and the specifier does not hide the procedure.
    implicit none
contains
    integer function newunit(unit) result(n)
        integer, intent(out), optional :: unit
        n = 10
        if (present(unit)) unit = n
    end function

    subroutine user_newunit()
        integer :: u, v, i
        open(newunit=u, file='file_open_16_a.txt', status='replace')
        if (u >= 0) error stop 1
        write(u, *) 42
        close(u)
        open(newunit=u, file='file_open_16_a.txt', status='old')
        read(u, *) i
        close(u, status='delete')
        if (i /= 42) error stop 2
        if (newunit(v) /= 10) error stop 3
        if (v /= 10) error stop 4
    end subroutine
end module

module file_open_16_external
    ! NEWUNIT= is not a procedure reference, so it is allowed under
    ! `implicit none (type, external)`.
    implicit none (type, external)
contains
    subroutine external_none()
        integer :: ios, i
        integer(8) :: u
        open(newunit=u, file='file_open_16_b.txt', status='replace', iostat=ios)
        if (ios /= 0) error stop 5
        if (u >= 0) error stop 6
        write(u, *) 7
        close(u)
        open(newunit=u, file='file_open_16_b.txt', status='old')
        read(u, *) i
        close(u, status='delete')
        if (i /= 7) error stop 7
    end subroutine
end module

program file_open_16
    use file_open_16_user, only: user_newunit
    use file_open_16_external, only: external_none
    implicit none (external)
    integer :: u, ios, i
    call user_newunit()
    call external_none()
    open(newunit=u, file='file_open_16_c.txt', status='replace', iostat=ios)
    if (ios /= 0) error stop 8
    write(u, *) 3
    close(u)
    open(newunit=u, file='file_open_16_c.txt', status='old')
    read(u, *) i
    close(u, status='delete')
    if (i /= 3) error stop 9
    print *, 'ok'
end program
