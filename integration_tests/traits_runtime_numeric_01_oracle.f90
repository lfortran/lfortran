! Standard-Fortran counterpart of traits_runtime_numeric_01.f90.
! A type-bound GENERIC binding over one deferred specific per member of
! integer | real(real64) models dynamic dispatch of the generic message
! sum{INumeric :: T}(x(:)) -> T. Abstract types replace traits. Every
! implementation passes its receiver first because an overriding binding must
! keep the deferred binding's passed-object position; the trait version also
! checks NOPASS and a named non-first receiver.
module traits_runtime_numeric_01_oracle_contracts_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: ISum, IStride

    type, abstract :: ISum
    contains
        procedure(sum_integer_interface), deferred :: sum_integer
        procedure(sum_real64_interface), deferred :: sum_real64
        generic :: sum => sum_integer, sum_real64
    end type ISum

    type, abstract :: IStride
    contains
        procedure(contiguity_interface), deferred, nopass :: contiguity
    end type IStride

    abstract interface
        function sum_integer_interface(self, x) result(s)
            import :: ISum
            class(ISum), intent(in) :: self
            integer,     intent(in) :: x(:)
            integer                 :: s
        end function sum_integer_interface

        function sum_real64_interface(self, x) result(s)
            import :: ISum, real64
            class(ISum),  intent(in) :: self
            real(real64), intent(in) :: x(:)
            real(real64)             :: s
        end function sum_real64_interface

        function contiguity_interface(x) result(code)
            import :: real64
            real(real64), intent(in) :: x(:)
            integer                  :: code
        end function contiguity_interface
    end interface
end module traits_runtime_numeric_01_oracle_contracts_m

module traits_runtime_numeric_01_oracle_providers_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_01_oracle_contracts_m, only: ISum, IStride
    implicit none
    private
    public :: ForwardSum, ScaledSum, SplitSum, StrideProbe, direct_code

    type, extends(ISum) :: ForwardSum
    contains
        procedure :: sum_integer => forward_integer
        procedure :: sum_real64 => forward_real64
    end type ForwardSum

    type, extends(ISum) :: ScaledSum
        integer :: factor = 1
    contains
        procedure :: sum_integer => scaled_integer
        procedure :: sum_real64 => scaled_real64
    end type ScaledSum

    type, extends(ISum) :: SplitSum
    contains
        procedure :: sum_integer => split_integer_binding
        procedure :: sum_real64 => split_real64_binding
    end type SplitSum

    type, extends(IStride) :: StrideProbe
    contains
        procedure, nopass :: contiguity => probe_contiguity
    end type StrideProbe

contains

    function forward_integer(self, x) result(s)
        class(ForwardSum), intent(in) :: self
        integer,           intent(in) :: x(:)
        integer                       :: s
        integer                       :: i
        s = int(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function forward_integer

    function forward_real64(self, x) result(s)
        class(ForwardSum), intent(in) :: self
        real(real64),      intent(in) :: x(:)
        real(real64)                  :: s
        integer                       :: i
        s = real(0, kind=real64)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function forward_real64

    function scaled_integer(self, x) result(s)
        class(ScaledSum), intent(in) :: self
        integer,          intent(in) :: x(:)
        integer                      :: s
        integer                      :: i
        s = int(0)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
        s = int(self%factor) * s
    end function scaled_integer

    function scaled_real64(self, x) result(s)
        class(ScaledSum), intent(in) :: self
        real(real64),     intent(in) :: x(:)
        real(real64)                 :: s
        integer                      :: i
        s = real(0, kind=real64)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
        s = real(self%factor, kind=real64) * s
    end function scaled_real64

    function split_integer_binding(self, x) result(s)
        class(SplitSum), intent(in) :: self
        integer,         intent(in) :: x(:)
        integer                     :: s
        s = split_integer(x)
    end function split_integer_binding

    function split_real64_binding(self, x) result(s)
        class(SplitSum), intent(in) :: self
        real(real64),    intent(in) :: x(:)
        real(real64)                :: s
        s = split_real64(x)
    end function split_real64_binding

    recursive function split_integer(x) result(s)
        integer, intent(in) :: x(:)
        integer             :: s
        integer             :: m
        if (size(x) == 0) then
            s = int(0)
        else if (size(x) == 1) then
            s = x(1)
        else
            m = size(x) / 2
            s = split_integer(x(:m)) + split_integer(x(m+1:))
        end if
    end function split_integer

    recursive function split_real64(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64)             :: s
        integer                  :: m
        if (size(x) == 0) then
            s = real(0, kind=real64)
        else if (size(x) == 1) then
            s = x(1)
        else
            m = size(x) / 2
            s = split_real64(x(:m)) + split_real64(x(m+1:))
        end if
    end function split_real64

    function probe_contiguity(x) result(code)
        real(real64), intent(in) :: x(:)
        integer                  :: code
        code = 0
        if (is_contiguous(x)) code = 1
    end function probe_contiguity

    ! Ordinary-call control for the same contiguity observation.
    function direct_code(x) result(code)
        real(real64), intent(in) :: x(:)
        integer                  :: code
        code = 0
        if (is_contiguous(x)) code = 1
    end function direct_code
end module traits_runtime_numeric_01_oracle_providers_m

module traits_runtime_numeric_01_oracle_consumers_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_01_oracle_contracts_m, only: ISum, IStride
    implicit none
    private
    public :: integer_total, real64_total, twice, stride_code

    interface twice
        module procedure twice_integer, twice_real64
    end interface twice

contains

    function integer_total(summer, x) result(r)
        class(ISum), intent(in) :: summer
        integer,     intent(in) :: x(:)
        integer                 :: r
        r = summer%sum(x)
    end function integer_total

    function real64_total(summer, x) result(r)
        class(ISum),  intent(in) :: summer
        real(real64), intent(in) :: x(:)
        real(real64)             :: r
        r = summer%sum_real64(x)
    end function real64_total

    function twice_integer(summer, x) result(r)
        class(ISum), intent(in) :: summer
        integer,     intent(in) :: x(:)
        integer                 :: r
        r = summer%sum(x) + summer%sum_integer(x)
    end function twice_integer

    function twice_real64(summer, x) result(r)
        class(ISum),  intent(in) :: summer
        real(real64), intent(in) :: x(:)
        real(real64)             :: r
        r = summer%sum(x) + summer%sum_real64(x)
    end function twice_real64

    function stride_code(probe, x) result(code)
        class(IStride), intent(in) :: probe
        real(real64),   intent(in) :: x(:)
        integer                    :: code
        code = probe%contiguity(x)
    end function stride_code
end module traits_runtime_numeric_01_oracle_consumers_m

program traits_runtime_numeric_01_oracle
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_01_oracle_contracts_m, only: ISum
    use traits_runtime_numeric_01_oracle_providers_m, only: ForwardSum, ScaledSum, &
        SplitSum, StrideProbe, direct_code
    use traits_runtime_numeric_01_oracle_consumers_m, only: integer_total, &
        real64_total, twice, stride_code
    implicit none
    integer, parameter :: lengths(9) = [0, 1, 2, 3, 4, 5, 7, 8, 9]
    integer :: xi(9), grid(3, 4), pass, k, choice, factor, j, checks
    real(real64) :: xr(9), gridr(3, 4), tenths(7)
    class(ISum), allocatable :: chosen
    type(ScaledSum) :: scaled
    type(StrideProbe) :: probe

    checks = 0
    xi = [3, -1, 4, 1, -5, 9, 2, -6, 5]
    xr = [1.5_real64, -0.25_real64, 2.75_real64, 0.5_real64, -3.0_real64, &
        4.25_real64, 1.0_real64, -2.5_real64, 3.125_real64]
    grid = reshape([(j, j = 1, 12)], [3, 4])
    gridr = 0.5_real64 * real(grid, real64)
    tenths = [(0.1_real64 * j, j = 1, 7)]

    call check_i(direct_code(xr(1:9:2)), 0)
    call check_i(direct_code(xr(2:6)), 1)
    call check_i(stride_code(probe, xr), 1)
    call check_i(stride_code(probe, xr(2:6)), 1)
    call check_i(stride_code(probe, xr(1:9:2)), 0)
    call check_i(stride_code(probe, xr(9:1:-1)), 0)
    call check_i(stride_code(probe, gridr(2, :)), 0)
    call check_i(stride_code(probe, gridr(:, 3)), 1)

    do pass = 1, 2
        do k = 1, 3
            choice = k
            if (pass == 2) choice = 4 - k
            select case (choice)
            case (1)
                chosen = ForwardSum()
                factor = 1
            case (2)
                allocate(chosen, source=ScaledSum(factor=3))
                factor = 3
            case (3)
                chosen = SplitSum()
                factor = 1
            end select
            call check_provider(chosen, factor)
            deallocate(chosen)
        end do
    end do

    scaled = ScaledSum(factor=5)
    call check_i(scaled%sum(xi), 60)
    call check_i(integer_total(scaled, xi), scaled%sum(xi))
    call check_r(real64_total(scaled, xr(::3)), scaled%sum(xr(::3)))
    call check_r(real64_total(scaled, xr(::3)), 15.0_real64)
    call check_i(integer_total(ForwardSum(), xi(2:8:3)), -12)
    call check_r(real64_total(SplitSum(), xr), 7.375_real64)

    print '(a,i0,a)', 'traits_runtime_numeric_01_oracle: ', checks, ' checks passed'

contains

    subroutine check_provider(summer, factor)
        class(ISum), intent(in) :: summer
        integer,     intent(in) :: factor
        integer :: i, n
        do i = 1, size(lengths)
            n = lengths(i)
            call check_i(integer_total(summer, xi(1:n)), factor * reference_i(xi(1:n)))
            call check_r(real64_total(summer, xr(1:n)), factor * reference_r(xr(1:n)))
            call check_i(summer%sum(xi(n:1:-1)), factor * reference_i(xi(n:1:-1)))
            call check_r(summer%sum(xr(1:n:2)), factor * reference_r(xr(1:n:2)))
            call check_i(twice(summer, xi(1:n)), 2 * factor * reference_i(xi(1:n)))
            call check_r(twice(summer, xr(n:1:-2)), 2 * factor * reference_r(xr(n:1:-2)))
        end do
        call check_i(summer%sum(grid(2, :)), factor * 26)
        call check_r(summer%sum(gridr(3, :)), factor * 15.0_real64)
        call check_i(summer%sum_integer(grid(:, 4)), factor * 33)
        call check_i(summer%sum(xi(1:3)) + 1, factor * 6 + 1)
        call check_r(2.0_real64 * summer%sum(xr(1:4)) - summer%sum(xr(1:2)), &
            factor * 7.75_real64)
        call check_close(summer%sum(tenths), factor * reference_r(tenths), &
            factor * 2.8_real64)
        if (kind(summer%sum(xr)) /= real64) error stop 31
        if (kind(summer%sum(xi)) /= kind(0)) error stop 32
    end subroutine check_provider

    function reference_i(x) result(s)
        integer, intent(in) :: x(:)
        integer :: s, i
        s = 0
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function reference_i

    function reference_r(x) result(s)
        real(real64), intent(in) :: x(:)
        real(real64) :: s
        integer :: i
        s = 0.0_real64
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function reference_r

    subroutine check_i(actual, expected)
        integer, intent(in) :: actual, expected
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,a,i0,a,i0)', 'check ', checks, ': ', actual, ' /= ', expected
            error stop 1
        end if
    end subroutine check_i

    ! Dyadic test data make every summation order exact.
    subroutine check_r(actual, expected)
        real(real64), intent(in) :: actual, expected
        checks = checks + 1
        if (actual /= expected) then
            print '(a,i0,a,es24.16,a,es24.16)', 'check ', checks, ': ', actual, &
                ' /= ', expected
            error stop 2
        end if
    end subroutine check_r

    ! Non-dyadic tenths: forward, backward and split orders may round
    ! differently. Allowed error: 16 * epsilon(1.0_real64) * magnitude.
    subroutine check_close(actual, expected, magnitude)
        real(real64), intent(in) :: actual, expected, magnitude
        checks = checks + 1
        if (.not. (abs(actual - expected) <= 16.0_real64 * epsilon(1.0_real64) * &
                abs(magnitude))) then
            print '(a,i0,a,es24.16,a,es24.16)', 'check ', checks, ': ', actual, &
                ' !~ ', expected
            error stop 3
        end if
    end subroutine check_close
end program traits_runtime_numeric_01_oracle
