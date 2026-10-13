! Standard-Fortran oracle for traits_protocol_01. The trait-constrained
! generic procedures are written out for each type argument the program uses
! (Mod7, integer, real(real64), real(real32)) with unchanged bodies; T(n)
! becomes Mod7(n), INT(n) or REAL(n, kind). Mod7's initializer is a generic
! interface named like the type, its static operator bindings are generic
! operator interfaces, and code is an ordinary type-bound function. The
! checks, their order and the summary line match the extension test.
module traits_protocol_01_oracle_mod7_m
    implicit none
    private
    public :: Mod7, operator(+), operator(*)

    type :: Mod7
        private
        integer :: r = 0
    contains
        procedure, pass :: code
    end type Mod7

    interface Mod7
        module procedure from_integer
    end interface Mod7

    interface operator(+)
        module procedure add
    end interface operator(+)

    interface operator(*)
        module procedure multiply, times_integer
    end interface operator(*)

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
        class(Mod7), intent(in) :: self
        integer                 :: c
        c = 100 + self%r
    end function code
end module traits_protocol_01_oracle_mod7_m

module traits_protocol_01_oracle_generic_m
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_protocol_01_oracle_mod7_m, only: Mod7, operator(+), operator(*)
    implicit none
    private
    public :: poly, power, twice_poly, series, coded_poly

    interface poly
        module procedure poly_mod7, poly_integer, poly_real64, poly_real32
    end interface poly
    interface power
        module procedure power_mod7, power_integer, power_real64
    end interface power
    interface twice_poly
        module procedure twice_poly_mod7, twice_poly_integer, twice_poly_real64
    end interface twice_poly
    interface series
        module procedure series_mod7, series_integer, series_real64, series_real32
    end interface series

contains

    function poly_mod7(x) result(r)
        type(Mod7), intent(in) :: x
        type(Mod7)             :: r
        r = Mod7(3) * x * x + x * 2 + Mod7(1)
    end function poly_mod7

    function poly_integer(x) result(r)
        integer, intent(in) :: x
        integer             :: r
        r = int(3) * x * x + x * 2 + int(1)
    end function poly_integer

    function poly_real64(x) result(r)
        real(real64), intent(in) :: x
        real(real64)             :: r
        r = real(3, real64) * x * x + x * 2 + real(1, real64)
    end function poly_real64

    function poly_real32(x) result(r)
        real(real32), intent(in) :: x
        real(real32)             :: r
        r = real(3, real32) * x * x + x * 2 + real(1, real32)
    end function poly_real32

    function power_mod7(x, k) result(r)
        type(Mod7), intent(in) :: x
        integer,    intent(in) :: k
        type(Mod7)             :: r
        integer                :: i
        r = Mod7(1)
        do i = 1, k
            r = r * x
        end do
    end function power_mod7

    function power_integer(x, k) result(r)
        integer, intent(in) :: x
        integer, intent(in) :: k
        integer             :: r
        integer             :: i
        r = int(1)
        do i = 1, k
            r = r * x
        end do
    end function power_integer

    function power_real64(x, k) result(r)
        real(real64), intent(in) :: x
        integer,      intent(in) :: k
        real(real64)             :: r
        integer                  :: i
        r = real(1, real64)
        do i = 1, k
            r = r * x
        end do
    end function power_real64

    function twice_poly_mod7(x) result(r)
        type(Mod7), intent(in) :: x
        type(Mod7)             :: r
        r = poly(x) + poly(x)
    end function twice_poly_mod7

    function twice_poly_integer(x) result(r)
        integer, intent(in) :: x
        integer             :: r
        r = poly(x) + poly(x)
    end function twice_poly_integer

    function twice_poly_real64(x) result(r)
        real(real64), intent(in) :: x
        real(real64)             :: r
        r = poly(x) + poly(x)
    end function twice_poly_real64

    function series_mod7(x, n) result(r)
        type(Mod7), intent(in) :: x
        integer,    intent(in) :: n
        type(Mod7)             :: r
        integer                :: i
        r = Mod7(0)
        do i = 1, n
            r = r + Mod7(i) * x
        end do
    end function series_mod7

    function series_integer(x, n) result(r)
        integer, intent(in) :: x
        integer, intent(in) :: n
        integer             :: r
        integer             :: i
        r = int(0)
        do i = 1, n
            r = r + int(i) * x
        end do
    end function series_integer

    function series_real64(x, n) result(r)
        real(real64), intent(in) :: x
        integer,      intent(in) :: n
        real(real64)             :: r
        integer                  :: i
        r = real(0, real64)
        do i = 1, n
            r = r + real(i, real64) * x
        end do
    end function series_real64

    function series_real32(x, n) result(r)
        real(real32), intent(in) :: x
        integer,      intent(in) :: n
        real(real32)             :: r
        integer                  :: i
        r = real(0, real32)
        do i = 1, n
            r = r + real(i, real32) * x
        end do
    end function series_real32

    function coded_poly(x) result(c)
        type(Mod7), intent(in) :: x
        integer                :: c
        type(Mod7)             :: p
        p = poly(x)
        c = p%code()
    end function coded_poly
end module traits_protocol_01_oracle_generic_m

program traits_protocol_01_oracle
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use traits_protocol_01_oracle_mod7_m, only: Mod7, operator(+), operator(*)
    use traits_protocol_01_oracle_generic_m, only: poly, power, twice_poly, series, coded_poly
    implicit none
    integer :: checks
    type(Mod7) :: a, b

    checks = 0

    a = Mod7(10)
    call check(a%code() == 103, 1)
    a = Mod7(-1)
    call check(a%code() == 106, 2)

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
end program traits_protocol_01_oracle
