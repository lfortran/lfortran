module template_deferred_subroutine_01_m
    implicit none

    template tm {t, write_value}
        deferred type :: t
        deferred interface
            subroutine write_value(x)
                type(t), intent(out) :: x
            end subroutine
        end interface
    contains
        subroutine run(x)
            type(t), intent(out) :: x
            integer :: y
            call write_value(x)
            call set_value(y)
            if (y /= 99) error stop
        contains
            subroutine set_value(y)
                integer, intent(out) :: y
                y = 99
            end subroutine
        end subroutine
    end template
contains
    subroutine set_value(x)
        integer, intent(out) :: x
        x = 17
    end subroutine
end module

program template_deferred_subroutine_01
    use template_deferred_subroutine_01_m
    implicit none
    instantiate tm {integer, set_value}, only: test => run
    integer :: x

    call test(x)
    if (x /= 17) error stop
    call check_local()
contains
    subroutine check_local()
        instantiate tm {integer, set_value}, only: local_test => run
        integer :: y
        call local_test(y)
        if (y /= 17) error stop
    end subroutine
end program
