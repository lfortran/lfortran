! Conformances to a runtime trait whose generic procedures are outside the
! erased subset of runtime generic messages (a vector subscript, a scalar
! intrinsic and forwarding a binder to its parent protocol) stay valid
! static code: they publish no runtime witness, and a later valid
! conformance of the same module keeps its runtime view.
module traits_protocol_static_04_contracts_m
    implicit none
    private
    public :: INumeric, IField, ISum, IScale

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

    abstract interface :: IScale
        function scaled{IField :: T}(x, w) result(s)
            type(T), intent(in) :: x(:)
            type(T), intent(in) :: w
            type(T)             :: s
        end function scaled
    end interface IScale
end module traits_protocol_static_04_contracts_m

module traits_protocol_static_04_intrinsics_m
    use traits_protocol_static_04_contracts_m, only: IField
    implicit none
    implements IField :: integer
    end implements integer
end module traits_protocol_static_04_intrinsics_m

module traits_protocol_static_04_providers_m
    use traits_protocol_static_04_contracts_m, only: INumeric, IField, ISum, IScale
    implicit none
    private
    public :: Gathered, Picked, Scaler, Counted, total, gathered_sum, picked_sum, scale_all

    type, sealed, implements(ISum) :: Gathered
    contains
        procedure, nopass :: sum => gathered_sum
    end type Gathered

    type, sealed, implements(ISum) :: Picked
    contains
        procedure, nopass :: sum => picked_sum
    end type Picked

    type, sealed, implements(IScale) :: Scaler
    contains
        procedure, nopass :: scaled => scale_all
    end type Scaler

    type, sealed, implements(ISum) :: Counted
    contains
        procedure, nopass :: sum => counted_sum
    end type Counted

contains

    function total{INumeric :: U}(x) result(s)
        type(U), intent(in) :: x(:)
        type(U)             :: s
        integer             :: i
        s = U(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function total

    ! A vector subscript of an erased array.
    function gathered_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = total(x([1, 3]))
    end function gathered_sum

    ! An intrinsic procedure of erased scalars.
    function picked_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = merge(x(size(x)), x(1), size(x) > 2)
    end function picked_sum

    ! IField's binder forwarded to a generic procedure requiring INumeric.
    function scale_all{IField :: T}(x, w) result(s)
        type(T), intent(in) :: x(:)
        type(T), intent(in) :: w
        type(T)             :: s
        s = total(x) * w
    end function scale_all

    function counted_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T)             :: s
        s = total(x(size(x):1:-1))
    end function counted_sum
end module traits_protocol_static_04_providers_m

program traits_protocol_static_04
    use traits_protocol_static_04_contracts_m, only: ISum
    use traits_protocol_static_04_intrinsics_m
    use traits_protocol_static_04_providers_m, only: Gathered, Picked, Scaler, &
        Counted, total, gathered_sum, picked_sum, scale_all
    implicit none
    class(ISum), allocatable :: v
    type(Gathered) :: g
    type(Picked)   :: p
    type(Scaler)   :: sc
    integer :: iv(4), k

    iv = [1, 20, 300, 4000]
    k = gathered_sum(iv)
    print '(a,i0)', 'static gathered_sum = ', k
    if (k /= 301) error stop 1
    if (g%sum(iv) /= 301) error stop 2
    if (picked_sum(iv) /= 4000) error stop 3
    if (p%sum(iv(1:2)) /= 1) error stop 4
    if (scale_all(iv, 2) /= 8642) error stop 5
    if (sc%scaled(iv(3:1:-2), 10) /= 3010) error stop 6
    if (total(iv) /= 4321) error stop 7

    v = Counted()
    k = v%sum(iv)
    print '(a,i0)', 'runtime counted sum = ', k
    if (k /= 4321) error stop 8
    if (v%sum(iv(2:3)) /= 320) error stop 9
    print '(a)', 'traits_protocol_static_04 ok'
end program traits_protocol_static_04
