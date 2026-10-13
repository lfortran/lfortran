! Standard-Fortran oracle of traits_infer_01: the generics written out for
! integer, Counter and real(8), with the declarations `:=` infers.
module traits_infer_01_oracle_m
    implicit none
    private
    public :: Counter, operator(+), total_integer, total_counter, larger_integer, &
        larger_real8

    type :: Counter
        integer :: n = 0
    end type Counter

    interface Counter
        module procedure counter_from
    end interface Counter
    interface operator(+)
        module procedure counter_plus
    end interface operator(+)

contains

    function counter_from(n) result(res)
        integer, intent(in) :: n
        type(Counter)       :: res
        res%n = n
    end function counter_from

    function counter_plus(lhs, rhs) result(res)
        type(Counter), intent(in) :: lhs, rhs
        type(Counter)             :: res
        res%n = lhs%n + rhs%n
    end function counter_plus

    function total_integer(x) result(s)
        integer, intent(in) :: x(:)
        integer             :: s, acc, first, next, i
        acc = 0
        do i = 1, size(x)
            first = x(1)
            next = acc + x(i)
            acc = next
        end do
        s = acc + first
    end function total_integer

    function total_counter(x) result(s)
        type(Counter), intent(in) :: x(:)
        type(Counter)             :: s, acc, first, next
        integer                   :: i
        acc = Counter(0)
        do i = 1, size(x)
            first = x(1)
            next = acc + x(i)
            acc = next
        end do
        s = acc + first
    end function total_counter

    function larger_integer(x, y) result(r)
        integer, intent(in) :: x, y
        integer             :: r, candidate
        candidate = x
        if (candidate < y) candidate = y
        r = candidate
    end function larger_integer

    function larger_real8(x, y) result(r)
        real(8), intent(in) :: x, y
        real(8)             :: r, candidate
        candidate = x
        if (candidate < y) candidate = y
        r = candidate
    end function larger_real8
end module traits_infer_01_oracle_m

program traits_infer_01_oracle
    use traits_infer_01_oracle_m
    implicit none
    type(Counter) :: c
    if (total_integer([1, 2, 3, 4]) /= 11) error stop 1
    c = total_counter([Counter(5), Counter(7)])
    if (c%n /= 17) error stop 2
    if (larger_integer(3, 8) /= 8) error stop 3
    if (larger_real8(2.5d0, 1.0d0) /= 2.5d0) error stop 4
    print *, "PASSED"
end program traits_infer_01_oracle
