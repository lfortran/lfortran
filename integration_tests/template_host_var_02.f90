module template_host_var_02_m
    implicit none
    type :: acc_t
        integer :: n = 0
        real :: total = 0.0
    end type
    type(acc_t) :: gacc
    integer :: gshared = 0
    integer :: gshadow = 100
    template tmpl {t}
        deferred type :: t
    contains
        subroutine add(x)
            type(t), intent(in) :: x
            gacc%n = gacc%n + 1
            gshared = gshared + 1
        end subroutine
        integer function shadow() result(r)
            integer :: gshadow
            gshadow = 5
            r = gshadow
        end function
    end template
end module

program template_host_var_02
    use template_host_var_02_m, only: tmpl, gacc, gshared, gshadow
    implicit none
    instantiate tmpl {integer}, only: addi => add, shadowi => shadow
    instantiate tmpl {real}, only: addr => add

    ! derived-type module variable, updated from two instantiations
    call addi(1)
    call addr(2.0)
    call addi(3)
    print *, gacc%n, gshared
    if (gacc%n /= 3) error stop
    if (gshared /= 3) error stop

    ! a local that shadows a module variable stays local
    if (shadowi() /= 5) error stop
    print *, gshadow
    if (gshadow /= 100) error stop
end program
