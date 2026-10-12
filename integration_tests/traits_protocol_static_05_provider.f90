! The separately compiled provider of traits_protocol_static_05. Vec's
! generic procedure gathers erased values with a vector subscript, outside
! the erased subset of runtime generic messages, so its conformance is
! static and publishes no runtime witness; Rev's conformance in the same
! module publishes one.
module traits_protocol_static_05_contracts_m
    implicit none
    private
    public :: INumeric, ISum

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface INumeric

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum
end module traits_protocol_static_05_contracts_m

module traits_protocol_static_05_intrinsics_m
    use traits_protocol_static_05_contracts_m, only: INumeric
    implicit none
    implements INumeric :: integer
    end implements integer
end module traits_protocol_static_05_intrinsics_m

module traits_protocol_static_05_provider_m
    use traits_protocol_static_05_contracts_m, only: INumeric, ISum
    implicit none
    private
    public :: Vec, Rev, vec_sum

    type, sealed, implements(ISum) :: Vec
    contains
        procedure, nopass :: sum => vec_sum
    end type Vec

    type, sealed, implements(ISum) :: Rev
    contains
        procedure, nopass :: sum => rev_sum
    end type Rev

contains

    function plain{INumeric :: U}(y) result(s)
        type(U), intent(in) :: y(:)
        type(U)             :: s
        integer             :: i
        s = U(0)
        do i = 1, size(y)
            s = s + y(i)
        end do
    end function plain

    function vec_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = plain(x([1, 3]))
    end function vec_sum

    function rev_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = plain(x(size(x):1:-1))
    end function rev_sum
end module traits_protocol_static_05_provider_m
