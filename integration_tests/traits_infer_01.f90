! `:=` declares local variables of a generic type parameter in its generic
! body: from an initializer, an operator requirement, an element of a
! dummy argument and a type-set value. Each specialization then holds them
! with the concrete type argument.
module traits_infer_01_m
    implicit none
    private
    public :: IAdd, Counter, total, larger

    abstract interface :: IAdd
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface IAdd

    type, sealed :: Counter
        integer :: n = 0
    end type Counter

    implements IAdd :: Counter
        initial :: counter_from
        procedure, static :: operator(+) => counter_plus
    end implements Counter

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

    function total{IAdd :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        acc := T(0)
        do i := 1, size(x)
            first := x(1)
            next := acc + x(i)
            acc = next
        end do
        s = acc + first
    end function total

    function larger{integer | real(8) :: T}(x, y) result(r)
        type(T), intent(in) :: x, y
        type(T)             :: r
        candidate := x
        if (candidate < y) candidate = y
        r = candidate
    end function larger
end module traits_infer_01_m

module traits_infer_01_intrinsics_m
    use traits_infer_01_m, only: IAdd
    implicit none
    implements IAdd :: integer
    end implements integer
end module traits_infer_01_intrinsics_m

program traits_infer_01
    use traits_infer_01_m, only: Counter, total, larger
    use traits_infer_01_intrinsics_m
    implicit none
    type(Counter) :: c
    if (total([1, 2, 3, 4]) /= 11) error stop 1
    c = total([Counter(5), Counter(7)])
    if (c%n /= 17) error stop 2
    if (larger(3, 8) /= 8) error stop 3
    if (larger(2.5d0, 1.0d0) /= 2.5d0) error stop 4
    print *, "PASSED"
end program traits_infer_01
