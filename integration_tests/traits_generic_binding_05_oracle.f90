! Standard-Fortran twin of traits_generic_binding_05.f90: the generic
! procedures become specific procedures under GENERIC type-bound bindings, and
! calls with explicit method type arguments become ordinary generic calls.
module traits_generic_binding_05_oracle_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: Plain, Child, Counter, Holder, Adder, next_index, calls

    type :: Plain
        integer :: tag = 1
    contains
        procedure, nopass :: total_i, total_r
        generic :: total => total_i, total_r
        procedure, nopass :: accumulate_i, accumulate_r
        generic :: accumulate => accumulate_i, accumulate_r
        procedure :: describe => plain_describe
    end type Plain

    type, extends(Plain) :: Child
    contains
        procedure :: describe => child_describe
    end type Child

    type :: Counter
        integer :: n = 2
    contains
        procedure, pass(self) :: first_n_i, first_n_r
        generic :: first_n => first_n_i, first_n_r
    end type Counter

    type :: Holder
        type(Plain)               :: plain
        type(Counter)             :: counters(3)
        class(Plain), allocatable :: any
    end type Holder

    type :: Adder
    contains
        procedure, nopass :: sum_i => adder_sum_i, sum_r => adder_sum_r
        generic :: sum => sum_i, sum_r
        procedure, nopass :: total_i, total_r
        generic :: total => total_i, total_r
    end type Adder

    integer :: calls = 0

contains

    integer function total_i(x) result(s)
        integer, intent(in) :: x(:)
        integer             :: i
        s = x(1)
        do i = 2, size(x)
            s = s + x(i)
        end do
    end function total_i

    real(real64) function total_r(x) result(s)
        real(real64), intent(in) :: x(:)
        integer                  :: i
        s = x(1)
        do i = 2, size(x)
            s = s + x(i)
        end do
    end function total_r

    subroutine accumulate_i(x, s)
        integer, intent(in)    :: x(:)
        integer, intent(inout) :: s
        integer                :: i
        do i = 1, size(x)
            s = s + x(i)
        end do
    end subroutine accumulate_i

    subroutine accumulate_r(x, s)
        real(real64), intent(in)    :: x(:)
        real(real64), intent(inout) :: s
        integer                     :: i
        do i = 1, size(x)
            s = s + x(i)
        end do
    end subroutine accumulate_r

    integer function first_n_i(x, self) result(s)
        integer,        intent(in) :: x(:)
        class(Counter), intent(in) :: self
        integer                    :: i
        s = x(1)
        do i = 2, self%n
            s = s + x(i)
        end do
    end function first_n_i

    real(real64) function first_n_r(x, self) result(s)
        real(real64),   intent(in) :: x(:)
        class(Counter), intent(in) :: self
        integer                    :: i
        s = x(1)
        do i = 2, self%n
            s = s + x(i)
        end do
    end function first_n_r

    integer function adder_sum_i(x) result(s)
        integer, intent(in) :: x(:)
        integer             :: i
        s = x(size(x))
        do i = size(x) - 1, 1, -1
            s = s + x(i)
        end do
    end function adder_sum_i

    real(real64) function adder_sum_r(x) result(s)
        real(real64), intent(in) :: x(:)
        integer                  :: i
        s = x(size(x))
        do i = size(x) - 1, 1, -1
            s = s + x(i)
        end do
    end function adder_sum_r

    integer function plain_describe(self)
        class(Plain), intent(in) :: self
        plain_describe = 10 * self%tag
    end function plain_describe

    integer function child_describe(self)
        class(Child), intent(in) :: self
        child_describe = 20 * self%tag
    end function child_describe

    integer function next_index()
        calls = calls + 1
        next_index = 2
    end function next_index

end module traits_generic_binding_05_oracle_m

program traits_generic_binding_05_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_generic_binding_05_oracle_m
    implicit none
    type(Plain)               :: p
    type(Plain)               :: ps(2)
    type(Child)               :: kid
    class(Plain), allocatable :: b
    type(Holder)              :: hold
    type(Counter)             :: cnt
    type(Adder)               :: adding
    integer                   :: xi(3) = [1, 2, 3]
    real(real64)              :: xr(3) = [1.5_real64, 2.5_real64, 3.0_real64]
    integer                   :: si
    real(real64)              :: sr

    allocate(Child :: b)
    allocate(Child :: hold%any)

    if (p%total(xi) /= 6) error stop 1
    if (abs(p%total(xr) - 7.0_real64) > 1.0e-12_real64) error stop 2
    if (kid%total(xi) /= 6) error stop 3
    if (b%total(xi) /= 6) error stop 4
    if (b%describe() /= 20) error stop 5
    if (ps(2)%total(xi) /= 6) error stop 6
    if (abs(hold%plain%total(xr) - 7.0_real64) > 1.0e-12_real64) error stop 7
    if (hold%any%total(xi) /= 6) error stop 8
    if (hold%any%describe() /= 20) error stop 9

    if (p%total(xi) /= 6) error stop 10
    if (abs(b%total(xr) - 7.0_real64) > 1.0e-12_real64) error stop 11
    if (ps(1)%total(xi) /= 6) error stop 12
    if (abs(hold%plain%total(xr) - 7.0_real64) > 1.0e-12_real64) error stop 13

    si = 4
    call p%accumulate(xi, si)
    if (si /= 10) error stop 14
    sr = 0.5_real64
    call hold%plain%accumulate(xr, sr)
    if (abs(sr - 7.5_real64) > 1.0e-12_real64) error stop 15
    si = 0
    call ps(2)%accumulate(xi, si)
    if (si /= 6) error stop 16
    si = 1
    call b%accumulate(s=si, x=xi)
    if (si /= 7) error stop 17

    if (cnt%first_n(xi) /= 3) error stop 18
    if (abs(cnt%first_n(xr) - 4.0_real64) > 1.0e-12_real64) error stop 19
    if (cnt%first_n(x=xi) /= 3) error stop 20
    hold%counters(2)%n = 3
    calls = 0
    if (hold%counters(next_index())%first_n(xi) /= 6) error stop 21
    if (calls /= 1) error stop 22
    if (abs(hold%counters(next_index())%first_n(xr) - 7.0_real64) &
            > 1.0e-12_real64) error stop 23
    if (calls /= 2) error stop 24

    if (adding%sum(xi) /= 6) error stop 25
    if (adding%total(xi) /= 6) error stop 26
    if (abs(adding%total(xr) - 7.0_real64) > 1.0e-12_real64) error stop 27

    print '(a)', 'traits_generic_binding_05 ok'
end program traits_generic_binding_05_oracle
