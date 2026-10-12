! An elemental requirement applied to an erased array inside a runtime
! generic message, count(x < x(1)), is outside the erased subset of runtime
! generic messages, so its conformance stays static: static calls work and
! no runtime witness is published. The same requirement applied to erased
! scalars keeps a runtime view.
module traits_protocol_static_06_contracts_m
    implicit none
    private
    public :: IOrd, ICount

    abstract interface :: IOrd
        initial(n)
            integer, intent(in) :: n
        end initial
        elemental function operator(<)(lhs, rhs) result(b)
            type(itself), intent(in) :: lhs, rhs
            logical                  :: b
        end function operator(<)
    end interface IOrd

    abstract interface :: ICount
        function below_first{IOrd :: T}(x) result(n)
            type(T), intent(in) :: x(:)
            integer             :: n
        end function below_first
    end interface ICount
end module traits_protocol_static_06_contracts_m

module traits_protocol_static_06_intrinsics_m
    use traits_protocol_static_06_contracts_m, only: IOrd
    implicit none
    implements IOrd :: integer
    end implements integer
end module traits_protocol_static_06_intrinsics_m

module traits_protocol_static_06_provider_m
    use traits_protocol_static_06_contracts_m, only: IOrd, ICount
    implicit none
    private
    public :: Counter, Looper, count_below

    type, sealed, implements(ICount) :: Counter
    contains
        procedure, nopass :: below_first => count_below
    end type Counter

    type, sealed, implements(ICount) :: Looper
    contains
        procedure, nopass :: below_first => loop_below
    end type Looper

contains

    function count_below{IOrd :: T}(x) result(n)
        type(T), intent(in) :: x(:)
        integer             :: n
        n = count(x < x(1))
    end function count_below

    function loop_below{IOrd :: T}(x) result(n)
        type(T), intent(in) :: x(:)
        integer             :: n
        integer             :: i
        n = 0
        do i = 2, size(x)
            if (x(i) < x(1)) n = n + 1
        end do
    end function loop_below
end module traits_protocol_static_06_provider_m

program traits_protocol_static_06
    use traits_protocol_static_06_contracts_m, only: ICount
    use traits_protocol_static_06_intrinsics_m
    use traits_protocol_static_06_provider_m, only: Counter, Looper, count_below
    implicit none
    class(ICount), allocatable :: v
    type(Counter) :: c
    integer :: iv(5), k

    iv = [3, 1, 4, 1, 5]
    k = count_below(iv)
    print '(a,i0)', 'static = ', k
    if (k /= 2) error stop 1
    if (c%below_first(iv) /= 2) error stop 2
    if (count_below(iv(5:1:-1)) /= 4) error stop 3

    v = Looper()
    k = v%below_first(iv)
    print '(a,i0)', 'runtime = ', k
    if (k /= 2) error stop 4
    if (v%below_first(iv([5, 3, 1])) /= 2) error stop 5
    if (v%below_first(iv(5:1:-1)) /= 4) error stop 6
    print '(a)', 'traits_protocol_static_06 ok'
end program traits_protocol_static_06
