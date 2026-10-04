! `instantiate` arguments naming procedures that are defined later in the
! same scoping unit or its host (#4853, #3148)
module template_instantiate_scope_10_tmpl
    implicit none
    private
    public :: search_t, apply_t

    requirement binary_predicate_r {T, lt}
        deferred type :: T
        deferred interface
            logical function lt(x, y)
                import :: T
                type(T), intent(in) :: x, y
            end function
        end interface
    end requirement

    template search_t {T, lt}
        require :: binary_predicate_r {T, lt}
    contains
        integer function count_less(a, v) result(r)
            type(T), intent(in) :: a(:), v
            integer :: i
            r = 0
            do i = 1, size(a)
                if (lt(a(i), v)) r = r + 1
            end do
        end function
    end template

    requirement unary_r {f}
        deferred interface
            integer function f(x)
                integer, intent(in) :: x
            end function
        end interface
    end requirement

    template apply_t {f}
        require :: unary_r {f}
    contains
        integer function apply(x)
            integer, intent(in) :: x
            apply = f(x)
        end function
    end template
end module

module template_instantiate_scope_10_mod
    use template_instantiate_scope_10_tmpl, only: apply_t
    implicit none
    ! `twice` is a procedure of this module, defined after `contains`
    instantiate apply_t {twice}, only: apply_twice => apply
contains
    integer function twice(x)
        integer, intent(in) :: x
        twice = 2*x
    end function
end module

program template_instantiate_scope_10
    use template_instantiate_scope_10_tmpl, only: search_t, apply_t
    use template_instantiate_scope_10_mod, only: apply_twice
    implicit none
    ! `ilt` is an internal procedure of this program
    instantiate search_t {integer, ilt}
    integer :: a(5) = [1, 3, 5, 7, 9]

    if (count_less(a, 6) /= 3) error stop
    if (apply_twice(21) /= 42) error stop
    call sub()
    print *, count_less(a, 6), apply_twice(21)

contains

    subroutine sub()
        ! `inc` is a later sibling internal procedure
        instantiate apply_t {inc}, only: apply_inc => apply
        if (apply_inc(4) /= 5) error stop
    end subroutine

    logical function ilt(x, y)
        integer, intent(in) :: x, y
        ilt = x < y
    end function

    integer function inc(x)
        integer, intent(in) :: x
        inc = x + 1
    end function
end program
