! A `procedure(iface) ::` dummy in an instantiated body keeps its interface:
! in a host-module helper that the template calls but the program does not
! import, with a deferred interface, and with an interface declared locally.
module template_procedure_dummy_01_m
    implicit none
    abstract interface
        integer function cb(x)
            integer, intent(in) :: x
        end function
        subroutine sub_cb(x)
            integer, intent(inout) :: x
        end subroutine
    end interface
    template t {op}
        deferred interface
            integer function op(x)
                integer, intent(in) :: x
            end function
        end interface
    contains
        integer function via_helper()
            via_helper = apply(twice)
        end function

        integer function via_sub_helper()
            integer :: x
            x = 5
            call apply_sub(bump, x)
            via_sub_helper = x
        end function

        integer function via_deferred(f)
            procedure(op) :: f
            via_deferred = f(3) + op(3)
        end function

        integer function via_local(f)
            interface
                integer function iface(x)
                    integer, intent(in) :: x
                end function
            end interface
            procedure(iface) :: f
            via_local = f(4)
        end function
    end template
contains
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function

    subroutine bump(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine

    integer function apply(f)
        procedure(cb) :: f
        apply = f(21)
    end function

    subroutine apply_sub(f, x)
        procedure(sub_cb) :: f
        integer, intent(inout) :: x
        call f(x)
    end subroutine
end module

program template_procedure_dummy_01
    use template_procedure_dummy_01_m, only: t, twice
    implicit none
    instantiate t {twice}
    if (via_helper() /= 42) error stop
    if (via_sub_helper() /= 6) error stop
    if (via_deferred(twice) /= 12) error stop
    if (via_local(twice) /= 8) error stop
    print *, "ok"
end program
