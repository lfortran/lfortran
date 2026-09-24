! Host variables of a template stay shared when a same-named local shadows them
! at the instantiation site: in a procedure of the hosting module, and in an
! internal procedure of a program that uses the module.
! A named constant of the program is copied, so a program-hosted template that
! uses it can also be instantiated in an internal procedure of the program.
module template_host_var_03_m
    implicit none
    integer :: hits = 0
    template tmpl {t}
        deferred type :: t
    contains
        subroutine bump()
            hits = hits + 3
        end subroutine
    end template
contains
    subroutine run()
        integer :: hits
        instantiate tmpl {integer}, only: bump
        hits = 50
        call bump()
        if (hits /= 50) error stop
    end subroutine
end module

program template_host_var_03
    use template_host_var_03_m, only: run, tmpl, hits
    implicit none
    integer, parameter :: np = 5
    template ptmpl {t}
        deferred type :: t
    contains
        function scaled(x) result(r)
            integer, intent(in) :: x
            integer :: r
            r = x * np
        end function
    end template
    call run()
    call run()
    print *, hits
    if (hits /= 6) error stop
    call s()
    print *, hits
    if (hits /= 9) error stop
contains
    subroutine s()
        integer :: hits
        instantiate tmpl {integer}, only: bump
        instantiate ptmpl {integer}, only: sc => scaled
        hits = 100
        call bump()
        if (hits /= 100) error stop
        print *, sc(2)
        if (sc(2) /= 10) error stop
    end subroutine
end program
