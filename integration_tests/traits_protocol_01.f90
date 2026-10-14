! Explicit numeric protocols (paper sect:tats and sect:overloading): a trait
! with an initializer requirement and operator requirements on itself,
! including an overloaded operator(*), is implemented by a sealed derived
! type through a retroactive implements block (initial and static operator
! bindings) and acknowledged by empty implements statements for integer,
! real(real64) and real(real32). Static generic code resolves T(n), the
! itself*itself and itself*integer overloads and operator(+) through the
! constraint, forwards T, and combines constraints with +. Residues modulo 7
! make a wrong operator, a wrong overload or the structure constructor in
! place of the initializer visible in the results.
module traits_protocol_01_contracts_m
    implicit none
    private
    public :: IRing, IShow

    abstract interface :: IRing
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs
            integer,      intent(in) :: rhs
            type(itself)             :: res
        end function operator(*)
    end interface IRing

    abstract interface :: IShow
        function code(self) result(c)
            type(itself), intent(in), pass :: self
            integer :: c
        end function code
    end interface IShow
end module traits_protocol_01_contracts_m

module traits_protocol_01_mod7_m
    use traits_protocol_01_contracts_m, only: IRing, IShow
    implicit none
    private

    type, public, sealed :: Mod7
        private
        integer :: r = 0
    end type Mod7

    implements (IRing + IShow) :: Mod7
        initial :: from_integer
        procedure, static :: operator(+) => add
        procedure, static :: operator(*) => multiply, times_integer
        procedure, pass   :: code
    end implements Mod7

contains

    function from_integer(n) result(res)
        integer, intent(in) :: n
        type(Mod7)          :: res
        res%r = modulo(n, 7)
    end function from_integer

    function add(lhs, rhs) result(res)
        type(Mod7), intent(in) :: lhs, rhs
        type(Mod7)             :: res
        res%r = modulo(lhs%r + rhs%r, 7)
    end function add

    function multiply(lhs, rhs) result(res)
        type(Mod7), intent(in) :: lhs, rhs
        type(Mod7)             :: res
        res%r = modulo(lhs%r * rhs%r, 7)
    end function multiply

    function times_integer(lhs, rhs) result(res)
        type(Mod7), intent(in) :: lhs
        integer,    intent(in) :: rhs
        type(Mod7)             :: res
        res%r = modulo(lhs%r * rhs, 7)
    end function times_integer

    function code(self) result(c)
        type(Mod7), intent(in) :: self
        integer                :: c
        c = 100 + self%r
    end function code
end module traits_protocol_01_mod7_m

module traits_protocol_01_intrinsics_m
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_protocol_01_contracts_m, only: IRing
    implicit none

    implements IRing :: integer
    end implements integer

    implements IRing :: real(real64)
    end implements real(real64)

    implements IRing :: real(real32)
    end implements real(real32)
end module traits_protocol_01_intrinsics_m

module traits_protocol_01_generic_m
    use traits_protocol_01_contracts_m, only: IRing, IShow
    implicit none
    private
    public :: poly, power, twice_poly, series, coded_poly

contains

    ! 3*x*x + x*2 + 1
    function poly{IRing :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = T(3) * x * x + x * 2 + T(1)
    end function poly

    function power{IRing :: T}(x, k) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: k
        type(T)             :: r
        integer             :: i
        r = T(1)
        do i = 1, k
            r = r * x
        end do
    end function power

    function twice_poly{IRing :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T)             :: r
        r = poly(x) + poly(x)
    end function twice_poly

    ! sum over i = 1..n of T(i) * x, with the initializer applied at run time
    function series{IRing :: T}(x, n) result(r)
        type(T), intent(in) :: x
        integer, intent(in) :: n
        type(T)             :: r
        integer             :: i
        r = T(0)
        do i = 1, n
            r = r + T(i) * x
        end do
    end function series

    function coded_poly{IRing + IShow :: T}(x) result(c)
        type(T), intent(in) :: x
        integer             :: c
        type(T)             :: p
        p = poly(x)
        c = p%code()
    end function coded_poly
end module traits_protocol_01_generic_m

program traits_protocol_01
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_protocol_01_mod7_m, only: Mod7
    use traits_protocol_01_intrinsics_m
    use traits_protocol_01_generic_m, only: poly, power, twice_poly, series, coded_poly
    implicit none
    integer :: checks
    type(Mod7) :: a, b

    checks = 0

    ! The initializer, not the structure constructor, defines Mod7(n).
    a = Mod7(10)
    call check(a%code() == 103, 1)
    a = Mod7(-1)
    call check(a%code() == 106, 2)

    ! Generic code over the derived type.
    b = poly(Mod7(3))
    call check(b%code() == 106, 3)
    b = poly(Mod7(10))
    call check(b%code() == 106, 4)
    b = power(Mod7(3), 6)
    call check(b%code() == 101, 5)
    b = power(Mod7(3), 0)
    call check(b%code() == 101, 6)
    b = twice_poly(Mod7(3))
    call check(b%code() == 105, 7)
    b = series(Mod7(2), 4)
    call check(b%code() == 106, 8)
    call check(coded_poly(Mod7(3)) == 106, 9)

    ! The same generic code over acknowledged intrinsic types uses the
    ! intrinsic initializer and operations, including integer * by rhs.
    call check(poly(3) == 34, 10)
    call check(poly(-2) == 9, 11)
    call check(poly(1.5_real64) == 10.75_real64, 12)
    call check(poly(0.5_real32) == 2.75_real32, 13)
    call check(kind(poly(0.5_real32)) == real32, 14)
    call check(kind(poly(1.5_real64)) == real64, 15)
    call check(power(2, 10) == 1024, 16)
    call check(power(1.5_real64, 2) == 2.25_real64, 17)
    call check(twice_poly(3) == 68, 18)
    call check(twice_poly(1.5_real64) == 21.5_real64, 19)
    call check(series(2, 4) == 20, 20)
    call check(series(0.5_real64, 4) == 5.0_real64, 21)
    call check(series(0.25_real32, 3) == 1.5_real32, 22)

    ! Static operator bindings also apply to concrete values where the
    ! conformance is visible, as the initializer overloads the constructor.
    b = Mod7(3) * Mod7(3)
    call check(b%code() == 102, 23)
    b = Mod7(3) * 4
    call check(b%code() == 105, 24)
    b = Mod7(3) + Mod7(5)
    call check(b%code() == 101, 25)
    b = Mod7(3) + Mod7(2) * 3
    call check(b%code() == 102, 26)

    print '(a,i0)', 'traits_protocol_01 checks: ', checks
    if (checks /= 26) error stop 99

contains

    subroutine check(condition, id)
        logical, intent(in) :: condition
        integer, intent(in) :: id
        if (.not. condition) then
            print '(a,i0)', 'traits_protocol_01 failed check ', id
            error stop 1
        end if
        checks = checks + 1
    end subroutine check
end program traits_protocol_01
