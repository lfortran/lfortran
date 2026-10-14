! LFortran traits extension test; GFortran does not accept this syntax.
! Explicit receivers of generic messages called at run time (TD-36). A trait
! object dispatches generic messages that are functions. IMeter's messages
! are generic over the closed type set integer | real(real64), so the view
! has one slot per member: record has an intent(inout) receiver, peak an
! intent(in) receiver after its generic argument, and forget an intent(out)
! receiver, which is reset on entry. IAccumulator's messages are generic over
! the open protocol INumeric, so the call passes the dictionary of its type
! argument: add_all has an intent(inout) receiver, sum_of an intent(in) one
! and restart an intent(out) one. The calls are made through borrowed views,
! an allocatable owner and a static generic procedure constrained by IMeter,
! with the generic argument by position and by keyword.
! traits_receiver_intent_07_oracle.f90 is the standard-Fortran counterpart.
module traits_receiver_intent_07_contracts_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: IMember, IMeter, INumeric, IAccumulator
    abstract interface :: IMember
        integer | real(real64)
    end interface IMember
    abstract interface :: IMeter
        integer function record{IMember :: T}(self, x)
            type(itself), intent(inout), pass :: self
            type(T), intent(in) :: x(:)
        end function
        function peak{IMember :: T}(x, self) result(s)
            type(T), intent(in) :: x(:)
            type(itself), intent(in), pass :: self
            type(T) :: s
        end function
        integer function forget{IMember :: T}(self, x)
            type(itself), intent(out), pass :: self
            type(T), intent(in) :: x
        end function
        integer function calls(self)
            type(itself), intent(in), pass :: self
        end function
    end interface IMeter
    abstract interface :: INumeric
        initial(n)
            integer, intent(in) :: n
        end initial
        function operator(+)(lhs, rhs) result(res)
            type(itself), intent(in) :: lhs, rhs
            type(itself)             :: res
        end function operator(+)
    end interface INumeric
    abstract interface :: IAccumulator
        integer function add_all{INumeric :: T}(self, x)
            type(itself), intent(inout), pass :: self
            type(T), intent(in) :: x(:)
        end function
        function sum_of{INumeric :: T}(self, x) result(s)
            type(itself), intent(in), pass :: self
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function
        integer function restart{INumeric :: T}(self, x)
            type(itself), intent(out), pass :: self
            type(T), intent(in) :: x(:)
        end function
        integer function seen(self)
            type(itself), intent(in), pass :: self
        end function
    end interface IAccumulator
end module traits_receiver_intent_07_contracts_m

module traits_receiver_intent_07_intrinsics_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_receiver_intent_07_contracts_m, only: INumeric
    implicit none
    implements INumeric :: integer
    end implements integer
    implements INumeric :: real(real64)
    end implements real(real64)
end module traits_receiver_intent_07_intrinsics_m

module traits_receiver_intent_07_providers_m
    use traits_receiver_intent_07_contracts_m, only: IMember, IMeter, INumeric, IAccumulator
    implicit none
    private
    public :: Meter, Acc
    type, public, sealed, implements(IMeter) :: Meter
        integer :: hits = 0
        integer :: items = 0
    contains
        procedure, pass :: record => meter_record
        procedure, pass(self) :: peak => meter_peak
        procedure, pass :: forget => meter_forget
        procedure, pass :: calls => meter_calls
    end type Meter
    type, public, sealed, implements(IAccumulator) :: Acc
        integer :: n = 0
    contains
        procedure, pass :: add_all => acc_add_all
        procedure, pass :: sum_of => acc_sum_of
        procedure, pass :: restart => acc_restart
        procedure, pass :: seen => acc_seen
    end type Acc
contains
    integer function meter_record{IMember :: T}(self, x) result(r)
        type(Meter), intent(inout) :: self
        type(T), intent(in) :: x(:)
        self%hits = self%hits + 1
        self%items = self%items + size(x)
        r = self%hits
    end function
    function meter_peak{IMember :: T}(x, self) result(s)
        type(T), intent(in) :: x(:)
        type(Meter), intent(in) :: self
        type(T) :: s
        integer :: i
        s = x(1)
        do i = 2, size(x)
            if (x(i) > s) s = x(i)
        end do
    end function
    integer function meter_forget{IMember :: T}(self, x) result(r)
        type(Meter), intent(out) :: self
        type(T), intent(in) :: x
        if (self%hits /= 0 .or. self%items /= 0) error stop 90
        self%hits = 1
        r = self%hits + self%items
    end function
    integer function meter_calls(self)
        type(Meter), intent(in) :: self
        meter_calls = self%hits
    end function
    integer function acc_add_all{INumeric :: T}(self, x) result(r)
        type(Acc), intent(inout) :: self
        type(T), intent(in) :: x(:)
        self%n = self%n + size(x)
        r = self%n
    end function
    function acc_sum_of{INumeric :: T}(self, x) result(s)
        type(Acc), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(0)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function
    integer function acc_restart{INumeric :: T}(self, x) result(r)
        type(Acc), intent(out) :: self
        type(T), intent(in) :: x(:)
        if (self%n /= 0) error stop 91
        self%n = size(x)
        r = self%n
    end function
    integer function acc_seen(self)
        type(Acc), intent(in) :: self
        acc_seen = self%n
    end function
end module traits_receiver_intent_07_providers_m

module traits_receiver_intent_07_client_m
    use, intrinsic :: iso_fortran_env, only: real64
    use traits_receiver_intent_07_contracts_m, only: IMeter, IAccumulator
    use traits_receiver_intent_07_intrinsics_m
    implicit none
contains
    subroutine use_meter(v)
        class(IMeter), intent(inout) :: v
        integer :: k
        k = v%record([1, 2, 3])
        if (k /= 1) error stop 1
        k = v%record(x=[0.5_real64, 2.5_real64])
        if (k /= 2 .or. v%calls() /= 2) error stop 2
        if (v%peak([4, 9, 2]) /= 9) error stop 3
        if (v%peak([0.5_real64, 2.5_real64]) /= 2.5_real64) error stop 4
        k = v%forget(7)
        if (k /= 1 .or. v%calls() /= 1) error stop 5
        k = v%forget(x=1.5_real64)
        if (k /= 1 .or. v%calls() /= 1) error stop 6
    end subroutine
    integer function read_meter(v)
        class(IMeter), intent(in) :: v
        read_meter = v%calls() + v%peak([5, 6])
    end function
    subroutine use_accumulator(a)
        class(IAccumulator), intent(inout) :: a
        integer :: k
        k = a%add_all([1, 2, 3, 4])
        if (k /= 4) error stop 7
        k = a%add_all([0.5_real64, 1.5_real64])
        if (k /= 6 .or. a%seen() /= 6) error stop 8
        if (a%sum_of([1, 2, 3, 4]) /= 10) error stop 9
        if (a%sum_of(x=[0.5_real64, 1.5_real64]) /= 2.0_real64) error stop 10
        k = a%restart([7, 8, 9])
        if (k /= 3 .or. a%seen() /= 3) error stop 11
    end subroutine
    subroutine meter_generic{IMeter :: M}(v)
        type(M), intent(inout) :: v
        integer :: k
        k = v%record([10, 20])
        if (k /= 2) error stop 12
        if (v%peak([1.5_real64, -1.0_real64]) /= 1.5_real64) error stop 13
    end subroutine
end module traits_receiver_intent_07_client_m

program traits_receiver_intent_07
    use traits_receiver_intent_07_contracts_m, only: IMeter
    use traits_receiver_intent_07_intrinsics_m
    use traits_receiver_intent_07_providers_m, only: Meter, Acc
    use traits_receiver_intent_07_client_m
    implicit none
    type(Meter) :: m
    type(Acc) :: a
    class(IMeter), allocatable :: owner
    call use_meter(m)
    if (m%hits /= 1 .or. m%items /= 0) error stop 21
    if (read_meter(m) /= 7) error stop 22
    call meter_generic(m)
    if (m%hits /= 2 .or. m%items /= 2) error stop 23
    owner = Meter()
    call use_meter(owner)
    if (owner%calls() /= 1 .or. read_meter(owner) /= 7) error stop 24
    call use_accumulator(a)
    if (a%n /= 3) error stop 25
    print '(a)', 'traits_receiver_intent_07 ok'
end program
