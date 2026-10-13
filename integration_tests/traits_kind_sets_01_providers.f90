! Providers of traits_kind_sets_01: a static generic over every numeric kind
! and one sealed type whose generic procedures fill the closed runtime slots.
module traits_kind_sets_01_providers
    use traits_kind_sets_01_contracts, only: INumeric, IExplicit, IOrdered, &
        ISum, ICount, IMax
    implicit none
    private
    public :: Summer, total, total_explicit

    type, sealed, implements(ISum + ICount + IMax) :: Summer
    contains
        procedure, nopass :: sum => summer_total
        procedure, nopass :: nonzero => summer_nonzero
        procedure, nopass :: largest => summer_largest
    end type Summer

contains

    function total{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function total

    function total_explicit{IExplicit :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function total_explicit

    function summer_total{IOrdered :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        integer             :: i
        s = T(0)
        do i = size(x), 1, -1
            s = s + x(i)
        end do
    end function summer_total

    function summer_nonzero{INumeric :: T}(x) result(n)
        type(T), intent(in) :: x(:)
        integer             :: n
        integer             :: i
        n = 0
        do i = 1, size(x)
            if (x(i) /= T(0)) n = n + 1
        end do
    end function summer_nonzero

    function summer_largest{IOrdered :: T}(x) result(m)
        type(T), intent(in) :: x(:)
        type(T)             :: m
        integer             :: i
        m = x(1)
        do i = 2, size(x)
            if (x(i) > m) m = x(i)
        end do
    end function summer_largest
end module traits_kind_sets_01_providers
