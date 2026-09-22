! A templated procedure reference written directly in a main program.
!
! The enclosing scope of such a reference is the Program, whose parent symbol
! table is the TranslationUnit rather than a symbol, so the "am I inside a
! template?" walk stops short. Both the function and the subroutine form are
! covered, with an intrinsic operator and with a named function as the
! instantiation argument.
!
! See integration_tests/template_simple_02.f90 for the same references made
! from inside a module procedure.

module template_program_01_m
    implicit none
    private
    public :: add_generic, add_sub, add_int

    requirement r {t, f}
        deferred type :: t
        deferred interface
            pure function f(x, y) result(z)
                type(t), intent(in) :: x, y
                type(t) :: z
            end function
        end interface
    end requirement

contains

    pure function add_int(x, y) result(z)
        integer, intent(in) :: x, y
        integer :: z
        z = x + y
    end function

    function add_generic {t, f} (x, y) result(z)
        require r {t, f}
        type(t), intent(in) :: x, y
        type(t) :: z
        z = f(x, y)
    end function

    subroutine add_sub {t, f} (x, y, z)
        require r {t, f}
        type(t), intent(in) :: x, y
        type(t), intent(out) :: z
        z = f(x, y)
    end subroutine

end module

program template_program_01
    use template_program_01_m
    implicit none
    integer :: s
    real :: rs

    ! function form, intrinsic operator argument
    s = add_generic{integer, operator(+)}(1, 2)
    print *, s
    if (s /= 3) error stop

    ! function form, named function argument
    s = add_generic{integer, add_int}(10, 20)
    print *, s
    if (s /= 30) error stop

    ! subroutine form
    call add_sub{integer, operator(+)}(3, 4, s)
    print *, s
    if (s /= 7) error stop

    ! a second instantiation of the same template, for a different type
    rs = add_generic{real, operator(+)}(1.5, 2.5)
    print *, rs
    if (abs(rs - 4.0) > 1e-6) error stop

end program
