! Standard-Fortran counterpart of traits_receiver_intent_07.f90: abstract
! base types whose generic bindings stand for the generic messages, with one
! specific deferred function per integer and real(real64) argument. The
! passed objects are intent(inout) for record and add_all, intent(in) for
! peak (passed second), calls, sum_of and seen, and intent(out) for forget
! and restart. They are called through class dummies, an allocatable owner
! and a direct call. It checks the same values.
module traits_receiver_intent_07_oracle_m
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private
    public :: IMeter, IAccumulator, Meter, Acc
    public :: use_meter, read_meter, use_accumulator, meter_direct
    type, abstract :: IMeter
    contains
        procedure(record_i_iface), deferred :: record_i
        procedure(record_r_iface), deferred :: record_r
        generic :: record => record_i, record_r
        procedure(peak_i_iface), deferred, pass(self) :: peak_i
        procedure(peak_r_iface), deferred, pass(self) :: peak_r
        generic :: peak => peak_i, peak_r
        procedure(forget_i_iface), deferred :: forget_i
        procedure(forget_r_iface), deferred :: forget_r
        generic :: forget => forget_i, forget_r
        procedure(calls_iface), deferred :: calls
    end type
    abstract interface
        integer function record_i_iface(self, x)
            import :: IMeter
            class(IMeter), intent(inout) :: self
            integer, intent(in) :: x(:)
        end function
        integer function record_r_iface(self, x)
            import :: IMeter, real64
            class(IMeter), intent(inout) :: self
            real(real64), intent(in) :: x(:)
        end function
        integer function peak_i_iface(x, self)
            import :: IMeter
            integer, intent(in) :: x(:)
            class(IMeter), intent(in) :: self
        end function
        real(real64) function peak_r_iface(x, self)
            import :: IMeter, real64
            real(real64), intent(in) :: x(:)
            class(IMeter), intent(in) :: self
        end function
        integer function forget_i_iface(self, x)
            import :: IMeter
            class(IMeter), intent(out) :: self
            integer, intent(in) :: x
        end function
        integer function forget_r_iface(self, x)
            import :: IMeter, real64
            class(IMeter), intent(out) :: self
            real(real64), intent(in) :: x
        end function
        integer function calls_iface(self)
            import :: IMeter
            class(IMeter), intent(in) :: self
        end function
    end interface
    type, abstract :: IAccumulator
    contains
        procedure(add_all_i_iface), deferred :: add_all_i
        procedure(add_all_r_iface), deferred :: add_all_r
        generic :: add_all => add_all_i, add_all_r
        procedure(sum_of_i_iface), deferred :: sum_of_i
        procedure(sum_of_r_iface), deferred :: sum_of_r
        generic :: sum_of => sum_of_i, sum_of_r
        procedure(restart_i_iface), deferred :: restart_i
        procedure(restart_r_iface), deferred :: restart_r
        generic :: restart => restart_i, restart_r
        procedure(seen_iface), deferred :: seen
    end type
    abstract interface
        integer function add_all_i_iface(self, x)
            import :: IAccumulator
            class(IAccumulator), intent(inout) :: self
            integer, intent(in) :: x(:)
        end function
        integer function add_all_r_iface(self, x)
            import :: IAccumulator, real64
            class(IAccumulator), intent(inout) :: self
            real(real64), intent(in) :: x(:)
        end function
        integer function sum_of_i_iface(self, x)
            import :: IAccumulator
            class(IAccumulator), intent(in) :: self
            integer, intent(in) :: x(:)
        end function
        real(real64) function sum_of_r_iface(self, x)
            import :: IAccumulator, real64
            class(IAccumulator), intent(in) :: self
            real(real64), intent(in) :: x(:)
        end function
        integer function restart_i_iface(self, x)
            import :: IAccumulator
            class(IAccumulator), intent(out) :: self
            integer, intent(in) :: x(:)
        end function
        integer function restart_r_iface(self, x)
            import :: IAccumulator, real64
            class(IAccumulator), intent(out) :: self
            real(real64), intent(in) :: x(:)
        end function
        integer function seen_iface(self)
            import :: IAccumulator
            class(IAccumulator), intent(in) :: self
        end function
    end interface
    type, extends(IMeter) :: Meter
        integer :: hits = 0
        integer :: items = 0
    contains
        procedure :: record_i => meter_record_i
        procedure :: record_r => meter_record_r
        procedure, pass(self) :: peak_i => meter_peak_i
        procedure, pass(self) :: peak_r => meter_peak_r
        procedure :: forget_i => meter_forget_i
        procedure :: forget_r => meter_forget_r
        procedure :: calls => meter_calls
    end type
    type, extends(IAccumulator) :: Acc
        integer :: n = 0
    contains
        procedure :: add_all_i => acc_add_all_i
        procedure :: add_all_r => acc_add_all_r
        procedure :: sum_of_i => acc_sum_of_i
        procedure :: sum_of_r => acc_sum_of_r
        procedure :: restart_i => acc_restart_i
        procedure :: restart_r => acc_restart_r
        procedure :: seen => acc_seen
    end type
contains
    integer function meter_record_i(self, x) result(r)
        class(Meter), intent(inout) :: self
        integer, intent(in) :: x(:)
        self%hits = self%hits + 1
        self%items = self%items + size(x)
        r = self%hits
    end function
    integer function meter_record_r(self, x) result(r)
        class(Meter), intent(inout) :: self
        real(real64), intent(in) :: x(:)
        self%hits = self%hits + 1
        self%items = self%items + size(x)
        r = self%hits
    end function
    integer function meter_peak_i(x, self) result(s)
        integer, intent(in) :: x(:)
        class(Meter), intent(in) :: self
        s = maxval(x)
    end function
    real(real64) function meter_peak_r(x, self) result(s)
        real(real64), intent(in) :: x(:)
        class(Meter), intent(in) :: self
        s = maxval(x)
    end function
    integer function meter_forget_i(self, x) result(r)
        class(Meter), intent(out) :: self
        integer, intent(in) :: x
        if (self%hits /= 0 .or. self%items /= 0) error stop 90
        self%hits = 1
        r = self%hits + self%items
    end function
    integer function meter_forget_r(self, x) result(r)
        class(Meter), intent(out) :: self
        real(real64), intent(in) :: x
        if (self%hits /= 0 .or. self%items /= 0) error stop 90
        self%hits = 1
        r = self%hits + self%items
    end function
    integer function meter_calls(self)
        class(Meter), intent(in) :: self
        meter_calls = self%hits
    end function
    integer function acc_add_all_i(self, x) result(r)
        class(Acc), intent(inout) :: self
        integer, intent(in) :: x(:)
        self%n = self%n + size(x)
        r = self%n
    end function
    integer function acc_add_all_r(self, x) result(r)
        class(Acc), intent(inout) :: self
        real(real64), intent(in) :: x(:)
        self%n = self%n + size(x)
        r = self%n
    end function
    integer function acc_sum_of_i(self, x) result(s)
        class(Acc), intent(in) :: self
        integer, intent(in) :: x(:)
        s = sum(x)
    end function
    real(real64) function acc_sum_of_r(self, x) result(s)
        class(Acc), intent(in) :: self
        real(real64), intent(in) :: x(:)
        s = sum(x)
    end function
    integer function acc_restart_i(self, x) result(r)
        class(Acc), intent(out) :: self
        integer, intent(in) :: x(:)
        if (self%n /= 0) error stop 91
        self%n = size(x)
        r = self%n
    end function
    integer function acc_restart_r(self, x) result(r)
        class(Acc), intent(out) :: self
        real(real64), intent(in) :: x(:)
        if (self%n /= 0) error stop 91
        self%n = size(x)
        r = self%n
    end function
    integer function acc_seen(self)
        class(Acc), intent(in) :: self
        acc_seen = self%n
    end function
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
    subroutine meter_direct(v)
        type(Meter), intent(inout) :: v
        integer :: k
        k = v%record([10, 20])
        if (k /= 2) error stop 12
        if (v%peak([1.5_real64, -1.0_real64]) /= 1.5_real64) error stop 13
    end subroutine
end module traits_receiver_intent_07_oracle_m

program traits_receiver_intent_07_oracle
    use traits_receiver_intent_07_oracle_m
    implicit none
    type(Meter) :: m
    type(Acc) :: a
    class(IMeter), allocatable :: owner
    call use_meter(m)
    if (m%hits /= 1 .or. m%items /= 0) error stop 21
    if (read_meter(m) /= 7) error stop 22
    call meter_direct(m)
    if (m%hits /= 2 .or. m%items /= 2) error stop 23
    allocate(owner, source=Meter())
    call use_meter(owner)
    if (owner%calls() /= 1 .or. read_meter(owner) /= 7) error stop 24
    call use_accumulator(a)
    if (a%n /= 3) error stop 25
    print '(a)', 'traits_receiver_intent_07 ok'
end program
