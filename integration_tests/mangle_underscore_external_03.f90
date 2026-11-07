! With --mangle-underscore-external, every global procedure name gets one
! trailing underscore, also a name that already ends in one, so that `f` and
! `f_` stay distinct (`f_` and `f__`, as with GFortran)
program mangle_underscore_external_03
    implicit none
    interface
        subroutine mue03_f(k)
            integer, intent(inout) :: k
        end subroutine
        subroutine mue03_f_(k)
            integer, intent(inout) :: k
        end subroutine
        integer function mue03_h_(n)
            integer, intent(in) :: n
        end function
        subroutine mue03_g(k)
            integer, intent(inout) :: k
        end subroutine
        subroutine mue03_g_(k)
            integer, intent(inout) :: k
        end subroutine
    end interface
    integer :: k
    k = 0
    call mue03_f(k)
    if (k /= 1) error stop
    call mue03_f_(k)
    if (k /= 101) error stop
    k = mue03_h_(k)
    if (k /= 303) error stop
    call mue03_g(k)
    if (k /= 313) error stop
    call mue03_g_(k)
    if (k /= 1313) error stop
    print *, k
end program

subroutine mue03_g(k)
    integer, intent(inout) :: k
    k = k + 10
end subroutine

subroutine mue03_g_(k)
    integer, intent(inout) :: k
    k = k + 1000
end subroutine
