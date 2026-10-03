module template_deferred_subroutine_02_m
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
            integer :: set_value
            set_value = 3
            call write_value(x)
            if (set_value /= 3) error stop
            set_value = 5
            call write_value(x)
            if (set_value /= 5) error stop
        end subroutine
    end template
contains
    subroutine set_value(x)
        integer, intent(out) :: x
        x = 17
    end subroutine
end module

program template_deferred_subroutine_02
    use template_deferred_subroutine_02_m
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
