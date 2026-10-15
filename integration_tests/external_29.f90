module external_29_m
    ! A module-scope interface block for the external function q
    implicit none
    interface
        integer function q(k)
            integer, intent(in) :: k
        end function q
    end interface
contains
    integer function useq(k)
        integer, intent(in) :: k
        useq = q(k) + 1
    end function useq
end module external_29_m

program external_29
    ! Interface blocks for external procedures defined after the program:
    ! in a module, in the program and in a BLOCK construct, including
    ! generic interfaces with a specific procedure of the same name.
    use external_29_m, only: useq
    implicit none
    interface
        subroutine s(y)
            integer, intent(inout) :: y
        end subroutine s
    end interface
    interface p
        integer function p(k)
            integer, intent(in) :: k
        end function p
        real function pr(x)
            real, intent(in) :: x
        end function pr
    end interface p
    integer :: r
    if (useq(2) /= 7) error stop 1
    r = 1
    call s(r)
    if (r /= 2) error stop 2
    if (p(2) /= 8) error stop 3
    if (abs(p(3.0) - 1.5) > 1e-6) error stop 4
    block
        interface t
            integer function t(k)
                integer, intent(in) :: k
            end function t
            real function tr(x)
                real, intent(in) :: x
            end function tr
        end interface t
        if (t(2) /= 10) error stop 5
        if (abs(t(3.0) - 0.75) > 1e-6) error stop 6
    end block
    print *, "ok"
end program external_29

integer function q(k)
    implicit none
    integer, intent(in) :: k
    q = 3*k
end function q

subroutine s(y)
    implicit none
    integer, intent(inout) :: y
    y = y + 1
end subroutine s

integer function p(k)
    implicit none
    integer, intent(in) :: k
    p = 4*k
end function p

real function pr(x)
    implicit none
    real, intent(in) :: x
    pr = x/2
end function pr

integer function t(k)
    implicit none
    integer, intent(in) :: k
    t = 5*k
end function t

real function tr(x)
    implicit none
    real, intent(in) :: x
    tr = x/4
end function tr
