! Calls of type-bound procedures bound to generic procedures that implement no
! trait message: the bound generic procedure's own constraints and argument
! list, without its passed-object dummy argument, are checked. Each case has
! its own scoping unit so continue-compilation reports exactly one diagnostic
! per case.
module traits_generic_binding_types
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
    type :: Plain
    contains
        procedure, nopass :: total
    end type Plain
    type :: Counter
        integer :: n = 2
    contains
        procedure, pass(self) :: first_n
    end type Counter
contains
    function total{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = x(1)
    end function total
    function first_n{INumeric :: T}(x, self) result(s)
        type(T),        intent(in) :: x(:)
        class(Counter), intent(in) :: self
        type(T)                    :: s
        s = x(self%n)
    end function first_n
end module

module traits_generic_binding_inferred
    use traits_generic_binding_types
    implicit none
contains
    function complex_total(p, c) result(r)
        type(Plain), intent(in) :: p
        complex(8), intent(in) :: c(:)
        complex(8) :: r
        r = p%total(c)
    end function
end module

module traits_generic_binding_explicit
    use traits_generic_binding_types
    implicit none
contains
    function explicit_complex_total(p, c) result(r)
        type(Plain), intent(in) :: p
        complex(8), intent(in) :: c(:)
        complex(8) :: r
        r = p%total{complex(8)}(c)
    end function
end module

module traits_generic_binding_category
    use traits_generic_binding_types
    implicit none
contains
    subroutine call_function(p, x)
        type(Plain), intent(in) :: p
        integer, intent(in) :: x(:)
        call p%total(x)
    end subroutine
end module

module traits_generic_binding_too_many
    use traits_generic_binding_types
    implicit none
contains
    subroutine passed_object_twice(cnt, x)
        type(Counter), intent(in) :: cnt
        integer, intent(in) :: x(:)
        print *, cnt%first_n(x, cnt)
    end subroutine
end module

module traits_generic_binding_missing
    use traits_generic_binding_types
    implicit none
contains
    subroutine missing_argument(cnt)
        type(Counter), intent(in) :: cnt
        print *, cnt%first_n()
    end subroutine
end module

module traits_generic_binding_keyword
    use traits_generic_binding_types
    implicit none
contains
    subroutine unknown_keyword(cnt, x)
        type(Counter), intent(in) :: cnt
        integer, intent(in) :: x(:)
        print *, cnt%first_n(y=x)
    end subroutine
end module

program continue_compilation_traits_generic_binding_01
    implicit none
end program
