module block_29_m
    implicit none
    interface g
        module procedure g_real
    end interface g
contains
    real function g_real(x)
        real, intent(in) :: x
        g_real = 10*x
    end function g_real
end module block_29_m

program block_29
    ! An abstract interface in a BLOCK construct for a procedure pointer,
    ! and a generic interface in a BLOCK construct with the name of a
    ! use-associated generic, which resolves the references that its own
    ! specific procedures do not match.
    use block_29_m, only: g
    implicit none
    block
        abstract interface
            integer function unary(k)
                integer, intent(in) :: k
            end function unary
        end interface
        interface
            integer function triple(k)
                integer, intent(in) :: k
            end function triple
        end interface
        procedure(unary), pointer :: p
        p => triple
        if (p(2) /= 6) error stop
    end block
    block
        interface g
            integer function g_int(k)
                integer, intent(in) :: k
            end function g_int
        end interface g
        if (g(2) /= 4) error stop
        if (abs(g(2.0) - 20.0) > 1e-6) error stop
    end block
    print *, "ok"
end program block_29

integer function triple(k)
    implicit none
    integer, intent(in) :: k
    triple = 3*k
end function triple

integer function g_int(k)
    implicit none
    integer, intent(in) :: k
    g_int = 2*k
end function g_int
