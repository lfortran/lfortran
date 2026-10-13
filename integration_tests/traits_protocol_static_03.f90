! A binder of a derived protocol (IField extends INumeric) forwarded to a
! generic helper and a runtime generic message that require the parent
! protocol. Statically, the conformance passes its INumeric dictionary to
! the runtime message.
module traits_protocol_static_03_contracts_m
    implicit none
    private
    public :: INumeric, IField, ISum

    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface INumeric

    abstract interface, extends(INumeric) :: IField
        function operator(*)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(*)
    end interface IField

    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T)             :: s
        end function sum
    end interface ISum
end module traits_protocol_static_03_contracts_m

module traits_protocol_static_03_intrinsics_m
    use traits_protocol_static_03_contracts_m, only: IField
    implicit none
    implements IField :: integer
    end implements integer
end module traits_protocol_static_03_intrinsics_m

module traits_protocol_static_03_provider_m
    use traits_protocol_static_03_contracts_m, only: INumeric, IField, ISum
    implicit none
    private
    public :: Adder, scaled_sum

    type, sealed, implements(ISum) :: Adder
    contains
        procedure, nopass :: sum => add_all
    end type Adder

contains

    function helper{INumeric :: U}(x) result(s)
        type(U), intent(in) :: x(:)
        type(U)             :: s
        integer             :: i
        s = U(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function helper

    function add_all{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = helper(x)
    end function add_all

    function scaled_sum{IField :: T}(inner, x, w) result(s)
        class(ISum), intent(in) :: inner
        type(T),     intent(in) :: x(:)
        type(T),     intent(in) :: w
        type(T)                 :: s
        s = inner%sum(x) * w + helper(x) * w
    end function scaled_sum
end module traits_protocol_static_03_provider_m

program traits_protocol_static_03
    use traits_protocol_static_03_contracts_m, only: ISum
    use traits_protocol_static_03_intrinsics_m
    use traits_protocol_static_03_provider_m, only: Adder, scaled_sum
    implicit none
    class(ISum), allocatable :: inner
    integer :: iv(3), k
    inner = Adder()
    iv = [1, 2, 3]
    k = scaled_sum(inner, iv, 10)
    print '(a,i0)', 'scaled_sum = ', k
    if (k /= 120) error stop 1
    k = scaled_sum(inner, iv(3:1:-2), 2)
    if (k /= 16) error stop 2
    print '(a)', 'traits_protocol_static_03 ok'
end program traits_protocol_static_03
