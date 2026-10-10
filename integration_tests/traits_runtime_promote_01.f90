! LFortran traits extension test; GFortran does not accept this syntax.
! With --fast, promote_allocatable_to_nonallocatable makes a local allocatable
! of constant extent a fixed-size array. Passed whole to a runtime trait
! message, it needs the descriptor cast that an ordinary call gets: closed
! member slots of generic messages (integer and real(real64), rank 1 and 2,
! nondefault lower bounds, an x(0:) dummy), a concrete function message and a
! concrete subroutine message with two array arguments. Static calls of the
! same providers are the ordinary-call control; an empty array and
! noncontiguous sections of the promoted arrays are further controls.
module traits_runtime_promote_01_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: INumeric, IProbe, Prober

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

    abstract interface :: IProbe
        function total{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function total
        function code0{INumeric :: T}(x) result(c)
            type(T), intent(in) :: x(0:)
            type(T)             :: c
        end function code0
        function total2{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:, :)
            type(T)             :: s
        end function total2
        function mean(x) result(m)
            import :: real64
            real(real64), intent(in) :: x(:)
            real(real64)             :: m
        end function mean
        subroutine show(x, y, k)
            import :: real64
            integer,      intent(in)  :: x(:)
            real(real64), intent(in)  :: y(:)
            integer,      intent(out) :: k
        end subroutine show
    end interface IProbe

    type, sealed, implements(IProbe) :: Prober
    contains
        procedure, nopass :: total => prober_total
        procedure, nopass :: code0 => prober_code0
        procedure, nopass :: total2 => prober_total2
        procedure, nopass :: mean => prober_mean
        procedure, nopass :: show => prober_show
    end type Prober

contains

    function prober_total{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function prober_total

    ! size + first element * 10000 + last element * 100, read through x(0:)
    function prober_code0{INumeric :: T}(x) result(c)
        type(T), intent(in) :: x(0:)
        type(T)             :: c
        c = T(size(x))
        if (size(x) > 0) c = c + x(0) * T(10000) + x(size(x) - 1) * T(100)
    end function prober_code0

    function prober_total2{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:, :)
        type(T)             :: s
        integer             :: i, j
        s = T(0)
        do j = 1, size(x, 2)
            do i = 1, size(x, 1)
                s = s + x(i, j) * T(i * 10 + j)
            end do
        end do
    end function prober_total2

    function prober_mean(x) result(m)
        real(real64), intent(in) :: x(:)
        real(real64)             :: m
        m = 0
        if (size(x) > 0) m = sum(x) / size(x)
    end function prober_mean

    ! Decimal digits: x(1), x(n), y(1), size(x), size(y), y(m).
    subroutine prober_show(x, y, k)
        integer,      intent(in)  :: x(:)
        real(real64), intent(in)  :: y(:)
        integer,      intent(out) :: k
        k = 100 * size(x) + 10 * size(y)
        if (size(x) > 0) k = k + 100000 * x(1) + 10000 * x(size(x))
        if (size(y) > 0) k = k + 1000 * nint(y(1)) + nint(y(size(y)))
    end subroutine prober_show

end module traits_runtime_promote_01_m

program traits_runtime_promote_01
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_runtime_promote_01_m
    implicit none
    type(Prober) :: p
    call run(p)
    print '(a)', 'ok'
contains
    subroutine run(v)
        class(IProbe), intent(in) :: v
        type(Prober) :: q
        integer, allocatable :: xi(:), a(:), e(:), m(:, :)
        real(real64), allocatable :: xr(:), r(:, :)
        integer :: i, j, k, kq
        allocate(xi(4), a(-2:4), e(0), m(2, 3), xr(2), r(0:2, -1:0))
        xi = [1, 2, 3, 4]
        xr = [1.0_real64, 3.0_real64]
        do i = -2, 4
            a(i) = i + 3
        end do
        do j = 1, 3
            do i = 1, 2
                m(i, j) = i + 2 * (j - 1)
            end do
        end do
        do j = -1, 0
            do i = 0, 2
                r(i, j) = real((i + 1) + 3 * (j + 1), real64)
            end do
        end do

        ! Closed member slots, whole promoted arrays.
        if (v%total(xi) /= 10) error stop 1
        if (v%total(xi) /= q%total(xi)) error stop 2
        if (v%total(xr) /= 4.0_real64) error stop 3
        if (v%total(xr) /= q%total(xr)) error stop 4
        if (v%code0(a) /= 1 * 10000 + 7 * 100 + 7) error stop 5
        if (v%code0(a) /= q%code0(a)) error stop 6
        if (v%total2(m) /= 1*11 + 2*21 + 3*12 + 4*22 + 5*13 + 6*23) error stop 7
        if (v%total2(m) /= q%total2(m)) error stop 8
        if (nint(v%total2(r)) /= 1*11 + 2*21 + 3*31 + 4*12 + 5*22 + 6*32) error stop 9
        if (v%total2(r) /= q%total2(r)) error stop 10
        if (v%total(e) /= 0 .or. q%total(e) /= 0) error stop 11

        ! Concrete function and subroutine messages.
        if (v%mean(xr) /= 2.0_real64) error stop 12
        if (v%mean(xr) /= q%mean(xr)) error stop 13
        call v%show(xi, xr, k)
        call q%show(xi, xr, kq)
        if (k /= 141423 .or. k /= kq) error stop 14
        call v%show(e, xr, k)
        if (k /= 1023) error stop 15

        ! Noncontiguous sections of the promoted arrays.
        if (v%total(xi(4:1:-2)) /= 6) error stop 16
        if (v%code0(a(4:-2:-3)) /= 7 * 10000 + 1 * 100 + 3) error stop 17
        call v%show(xi(1:4:3), xr(2:1:-1), k)
        if (k /= 143221) error stop 18
    end subroutine run
end program traits_runtime_promote_01
