program block_32
    ! A generic interface in a BLOCK construct is built once all the
    ! declarations of the BLOCK are known: its specific q is the external
    ! procedure declared by the interface body of the BLOCK, which hides the
    ! internal procedure q of the host, even when the interface body follows
    ! the generic interface.
    implicit none
    block
        interface g
            procedure :: q
        end interface
        interface
            integer function q(k)
                integer, intent(in) :: k
            end function q
        end interface
        if (g(2) /= 6) error stop
    end block
    block
        interface
            integer function q(k)
                integer, intent(in) :: k
            end function q
        end interface
        interface g
            procedure :: q
        end interface
        if (g(2) /= 6) error stop
    end block
    if (q(2) /= 2000) error stop
    print *, "ok"
contains
    integer function q(k)
        integer, intent(in) :: k
        q = 1000*k
    end function q
end program block_32

integer function q(k)
    implicit none
    integer, intent(in) :: k
    q = 3*k
end function q
