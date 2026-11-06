! A formatted READ into a character variable that is a pointer, or that is
! host associated (accessed through a pointer to the host's variable), stores
! into the variable (#16262).
module read_120_mod
    implicit none
contains
    subroutine host(res)
        character(len=6), intent(out) :: res
        character(len=6) :: s
        character(len=3) :: t
        t = 'xyz'
        call inner()
        res = s
    contains
        subroutine inner()
            read(t, '(A6)') s
        end subroutine
    end subroutine
end module

program read_120
    use read_120_mod, only: host
    implicit none
    character(len=8) :: line, line2, line3
    character(len=8), target :: tgt
    character(len=:), pointer :: p
    character(len=6) :: res
    character(len=3) :: src = 'abc'
    integer :: u

    call read_a8()
    if (line /= 'abc') error stop 1

    call read_a()
    if (line2 /= 'abc') error stop 2

    open(newunit=u, status='scratch')
    write(u, '(A)') 'hello'
    rewind(u)
    call read_unit()
    close(u)
    if (line3 /= 'hello') error stop 3

    p => tgt
    read(src, '(A8)') p
    if (tgt /= 'abc') error stop 4

    call host(res)
    if (res /= 'xyz') error stop 5

    print '(5A)', '[', line, line2, line3, ']'
contains
    subroutine read_a8()
        read(src, '(A8)') line
    end subroutine

    subroutine read_a()
        read(src, '(A)') line2
    end subroutine

    subroutine read_unit()
        read(u, '(A)') line3
    end subroutine
end program
