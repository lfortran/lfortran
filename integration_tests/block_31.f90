program block_31
    ! A procedure pointer in a BLOCK construct of an internal subroutine,
    ! with an abstract interface that imports the host's n.
    implicit none
    integer :: n
    n = 3
    call work()
    print *, "ok"
contains
    subroutine s(a)
        integer, intent(inout) :: a(n)
        a = a + 1
    end subroutine s
    subroutine work()
        integer :: x(3)
        x = [1, 2, 3]
        block
            abstract interface
                subroutine iface(a)
                    import :: n
                    integer, intent(inout) :: a(n)
                end subroutine iface
            end interface
            procedure(iface), pointer :: p
            p => s
            call p(x)
        end block
        if (any(x /= [2, 3, 4])) error stop
    end subroutine work
end program block_31
