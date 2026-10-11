! LFortran traits extension test; GFortran does not accept this syntax.
! Runtime dispatch of the generic message sum{INumeric :: T}(x(:)) -> T over
! the finite numeric type set integer | real(real64). Calls go through
! borrowed views, owners and a generic consumer, with whole, empty, odd/even,
! strided, reversed and 2-D row sections, explicit and inferred type
! arguments, and scalar T results in expressions. Each type-set member uses
! the provider's own compiled entry. The contiguity probe also checks that an
! ordinary runtime array argument is not copied into contiguous storage.
! Standard counterpart: traits_runtime_numeric_01_oracle.f90.
module traits_runtime_numeric_01_contracts_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, ISum, IStride

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum

    abstract interface :: IStride
        function contiguity(x) result(code)
            real(real64), intent(in) :: x(:)
            integer                  :: code
        end function contiguity
    end interface IStride
end module traits_runtime_numeric_01_contracts_m

module traits_runtime_numeric_01_providers_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_01_contracts_m, only: INumeric, ISum, IStride
    implicit none
    private
    public :: ForwardSum, ScaledSum, SplitSum, StrideProbe, direct_code

    type, sealed, implements(ISum) :: ForwardSum
    contains
        procedure, nopass :: sum => forward_total
    end type ForwardSum

    type, sealed, implements(ISum) :: ScaledSum
        integer :: factor = 1
    contains
        procedure, pass(self) :: sum => scaled_total
    end type ScaledSum

    type, sealed, implements(ISum) :: SplitSum
    contains
        procedure, nopass :: sum => split_total
    end type SplitSum

    type, sealed, implements(IStride) :: StrideProbe
    contains
        procedure, nopass :: contiguity => probe_contiguity
    end type StrideProbe

contains

    function forward_total{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function forward_total

    function scaled_total{INumeric :: T}(x, self) result(s)
        type(T),         intent(in) :: x(:)
        type(ScaledSum), intent(in) :: self
        type(T)                     :: s
        integer                     :: i
        s = T(0)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
        s = T(self%factor) * s
    end function scaled_total

    recursive function split_total{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: m
        if (size(x) == 0) then
            s = T(0)
        else if (size(x) == 1) then
            s = x(1)
        else
            m = size(x) / 2
            s = split_total(x(:m)) + split_total(x(m+1:))
        end if
    end function split_total

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
end module traits_runtime_numeric_01_providers_m

module traits_runtime_numeric_01_consumers_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_01_contracts_m, only: INumeric, ISum, IStride
    implicit none
    private
    public :: integer_total, real64_total, twice, stride_code

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
        r = summer%sum{real(real64)}(x)
    end function real64_total

    ! Checked once as a template; each static specialization selects the
    ! runtime slot of its own type-set member.
    function twice{INumeric :: T}(summer, x) result(r)
        class(ISum), intent(in) :: summer
        type(T),     intent(in) :: x(:)
        type(T)                 :: r
        r = summer%sum(x) + summer%sum{T}(x)
    end function twice

    function stride_code(probe, x) result(code)
        class(IStride), intent(in) :: probe
        real(real64),   intent(in) :: x(:)
        integer                    :: code
        code = probe%contiguity(x)
    end function stride_code
end module traits_runtime_numeric_01_consumers_m

program traits_runtime_numeric_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_numeric_01_contracts_m, only: ISum
    use traits_runtime_numeric_01_providers_m, only: ForwardSum, ScaledSum, &
        SplitSum, StrideProbe, direct_code
    use traits_runtime_numeric_01_consumers_m, only: integer_total, &
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

    print '(a,i0,a)', 'traits_runtime_numeric_01: ', checks, ' checks passed'

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
        call check_i(summer%sum{integer}(grid(:, 4)), factor * 33)
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
end program traits_runtime_numeric_01
