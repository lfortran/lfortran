! Standard-Fortran oracle for traits_generic_requirement_01.f90: the generic
! procedure twice_sum is concretized for each (Adder, Elem) pair it is called
! with, the T-generic leaf methods become generic bindings, and explicit
! type arguments select the same concrete procedures. 60 checks.
module traits_generic_requirement_01_oracle_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: SimpleSum, ScaledSum, twice_sum

    type :: SimpleSum
    contains
        procedure, nopass :: sum_i => simple_sum_i
        procedure, nopass :: sum_r => simple_sum_r
        generic :: sum => sum_i, sum_r
    end type SimpleSum

    type :: ScaledSum
        integer :: factor = 1
    contains
        procedure, pass :: sum_i => scaled_sum_i
        procedure, pass :: sum_r => scaled_sum_r
        generic :: sum => sum_i, sum_r
    end type ScaledSum

    interface twice_sum
        module procedure twice_simple_i, twice_simple_r
        module procedure twice_scaled_i, twice_scaled_r
    end interface twice_sum

contains

    function simple_sum_i(x) result(s)
        integer, intent(in) :: x(:)
        integer :: s, i
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum_i

    function simple_sum_r(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: i
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function simple_sum_r

    function scaled_sum_i(self, x) result(s)
        class(ScaledSum), intent(in) :: self
        integer, intent(in) :: x(:)
        integer :: s
        s = self%factor * simple_sum_i(x)
    end function scaled_sum_i

    function scaled_sum_r(self, x) result(s)
        class(ScaledSum), intent(in) :: self
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        s = real(self%factor, real64) * simple_sum_r(x)
    end function scaled_sum_r

    function twice_simple_i(a, x) result(s)
        type(SimpleSum), intent(in) :: a
        integer, intent(in) :: x(:)
        integer :: s
        s = a%sum(x) + a%sum(x)
    end function twice_simple_i

    function twice_simple_r(a, x) result(s)
        type(SimpleSum), intent(in) :: a
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        s = a%sum(x) + a%sum(x)
    end function twice_simple_r

    function twice_scaled_i(a, x) result(s)
        type(ScaledSum), intent(in) :: a
        integer, intent(in) :: x(:)
        integer :: s
        s = a%sum(x) + a%sum(x)
    end function twice_scaled_i

    function twice_scaled_r(a, x) result(s)
        type(ScaledSum), intent(in) :: a
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        s = a%sum(x) + a%sum(x)
    end function twice_scaled_r
end module traits_generic_requirement_01_oracle_m

program traits_generic_requirement_01_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_requirement_01_oracle_m, only: SimpleSum, ScaledSum, twice_sum
    implicit none
    integer, allocatable :: xi(:)
    real(real64), allocatable :: xr(:)
    integer :: n, i, base, checks

    checks = 0
    do n = 0, 9
        xi = [(i, i = 1, n)]
        xr = [(real(i, real64), i = 1, n)]
        base = n * (n + 1) / 2
        call check_i(twice_sum(SimpleSum(), xi), 2 * base, 1)
        call check_r(twice_sum(SimpleSum(), xr), 2 * base, 2)
        call check_i(twice_sum(ScaledSum(3), xi), 6 * base, 3)
        call check_r(twice_sum(ScaledSum(3), xr), 6 * base, 4)
        call check_i(twice_sum(SimpleSum(), xi), 2 * base, 5)
        call check_r(twice_sum(ScaledSum(2), xr), 4 * base, 6)
    end do
    if (checks /= 60) error stop 99
    print '(a,i0)', 'traits_generic_requirement_01 checks: ', checks

contains

    subroutine check_i(actual, expected, code)
        integer, intent(in) :: actual, expected, code
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,a,i0,a,i0)', 'check ', code, ': ', actual, ' /= ', expected
            error stop code
        end if
    end subroutine check_i

    subroutine check_r(actual, expected, code)
        real(real64), intent(in) :: actual
        integer, intent(in) :: expected, code
        checks = checks + 1
        if (actual /= real(expected, real64)) then
            print '(a,i0,a,g0,a,i0)', 'check ', code, ': ', actual, ' /= ', expected
            error stop code
        end if
    end subroutine check_r

end program traits_generic_requirement_01_oracle
