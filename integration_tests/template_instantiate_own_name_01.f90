! Instantiating a templated subprogram under its own name replaces the
! use-associated name of the template with the instantiated procedure.
module template_instantiate_own_name_01_m
    implicit none
contains
    template function g{t}(x)
        deferred type :: t
        type(t), intent(in) :: x
        type(t) :: g
        g = x
    end function

    template subroutine s{t}(x, y)
        deferred type :: t
        type(t), intent(in) :: x
        type(t), intent(out) :: y
        y = x
    end subroutine
end module

module template_instantiate_own_name_01_m2
    use template_instantiate_own_name_01_m, only: s
    implicit none
    instantiate s {real}, only: s
end module

program template_instantiate_own_name_01
    use template_instantiate_own_name_01_m, only: g
    implicit none
    instantiate g {integer}, only: g => g
    if (g(4) /= 4) error stop
    call test_subroutine()
    call test_no_only_list()
    call test_module()
    print *, "ok"
contains
    subroutine test_subroutine()
        use template_instantiate_own_name_01_m, only: s
        instantiate s {integer}, only: s
        integer :: y
        call s(5, y)
        if (y /= 5) error stop
    end subroutine

    subroutine test_no_only_list()
        use template_instantiate_own_name_01_m, only: s
        instantiate s {integer}
        integer :: y
        call s(6, y)
        if (y /= 6) error stop
    end subroutine

    subroutine test_module()
        use template_instantiate_own_name_01_m2, only: s
        real :: y
        call s(2.5, y)
        if (abs(y - 2.5) > 1e-6) error stop
    end subroutine
end program
