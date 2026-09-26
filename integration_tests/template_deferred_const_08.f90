! A deferred constant declared after a deferred subroutine interface in the
! specification part of a template. The interface body must not end the
! template's context, so the later `deferred` declaration is still accepted.

module template_deferred_const_08_m
    implicit none
    private
    public :: test_add

    integer, parameter :: three = 3

    template tmpl {n, op}
        deferred interface
            subroutine op(x)
                integer, intent(inout) :: x
            end subroutine
        end interface
        deferred integer, parameter :: n
        private
        public :: apply_n
    contains
        subroutine apply_n(x)
            integer, intent(inout) :: x
            integer :: i
            do i = 1, n
                call op(x)
            end do
        end subroutine
    end template

contains

    subroutine add_two(x)
        integer, intent(inout) :: x
        x = x + 2
    end subroutine

    subroutine test_add()
        instantiate tmpl {three, add_two}, only: apply_3 => apply_n
        integer :: x
        x = 1
        call apply_3(x)
        if (x /= 7) error stop
        print *, x
    end subroutine

end module

program template_deferred_const_08
    use template_deferred_const_08_m, only: test_add
    implicit none
    call test_add()
end program
