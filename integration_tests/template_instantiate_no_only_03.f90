! A named generic interface of a template, instantiated without an only-list,
! extends a generic interface of the same name accessible in the scope, and
! is extended by an interface block of the scope (#13919).
module template_instantiate_no_only_03_m
    implicit none
    interface dbl
        module procedure dbl_char
    end interface
    template tt {t, plus}
        deferred type :: t
        deferred interface
            function plus(x, y) result(z)
                type(t), intent(in) :: x, y
                type(t) :: z
            end function
        end interface
        interface dbl
            procedure twice
        end interface
    contains
        function twice(x) result(z)
            type(t), intent(in) :: x
            type(t) :: z
            z = plus(x, x)
        end function
    end template
    template uu {u, mul}
        deferred type :: u
        deferred interface
            function mul(x, y) result(z)
                type(u), intent(in) :: x, y
                type(u) :: z
            end function
        end interface
        interface dbl
            procedure square
        end interface
    contains
        function square(x) result(z)
            type(u), intent(in) :: x
            type(u) :: z
            z = mul(x, x)
        end function
    end template
contains
    function dbl_char(x) result(z)
        character(*), intent(in) :: x
        character(2*len(x)) :: z
        z = x // x
    end function
    function iadd(x, y) result(z)
        integer, intent(in) :: x, y
        integer :: z
        z = x + y
    end function
    function rmul(x, y) result(z)
        real, intent(in) :: x, y
        real :: z
        z = x * y
    end function
    function dadd(x, y) result(z)
        real(8), intent(in) :: x, y
        real(8) :: z
        z = x + y
    end function
end module

program template_instantiate_no_only_03
    use template_instantiate_no_only_03_m
    implicit none
    interface dbl
        procedure ldbl
    end interface
    instantiate tt {integer, iadd}
    instantiate uu {real, rmul}
    if (dbl(4) /= 8) error stop
    if (abs(dbl(1.5) - 2.25) > 1e-6) error stop
    if (dbl("ab") /= "abab") error stop
    if (dbl(.true.)) error stop
    call host()
    print *, dbl(4), dbl(1.5), dbl("ab")
contains
    logical function ldbl(x)
        logical, intent(in) :: x
        ldbl = .not. x
    end function
    subroutine host()
        instantiate tt {real(8), dadd}
        if (abs(dbl(3.0_8) - 6.0_8) > 1e-12_8) error stop
        if (abs(twice(2.0_8) - 4.0_8) > 1e-12_8) error stop
        if (abs(dbl(3.0) - 9.0) > 1e-6) error stop
        if (dbl(4) /= 8) error stop
        if (dbl("xy") /= "xyxy") error stop
    end subroutine
end program
