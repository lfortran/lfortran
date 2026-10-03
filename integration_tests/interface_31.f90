! Test: abstract interface defined after the generic interface that uses it.
! Fortran allows forward references within a module specification section.
module interface_31_mod
    implicit none

    type :: result_t
        integer :: status = 0
    end type

    abstract interface
        subroutine callback_iface(x)
            integer, intent(in) :: x
        end subroutine
    end interface

    interface run
        module function run_impl(cmd, callback) result(res)
            character(*), intent(in) :: cmd
            procedure(callback_iface), optional :: callback
            type(result_t) :: res
        end function
    end interface
end module

submodule (interface_31_mod) interface_31_sub
contains
    module function run_impl(cmd, callback) result(res)
        character(*), intent(in) :: cmd
        procedure(callback_iface), optional :: callback
        type(result_t) :: res
        res%status = 1
        if (present(callback)) then
            call callback(42)
            res%status = 2
        end if
    end function
end submodule

program interface_31
    use interface_31_mod, only: result_t, run
    implicit none
    type(result_t) :: r

    r = run("test", callback=my_cb)
    if (r%status /= 2) error stop

    r = run("test2")
    if (r%status /= 1) error stop

    print *, "PASS"
contains
    subroutine my_cb(x)
        integer, intent(in) :: x
        if (x /= 42) error stop
    end subroutine
end program
