! Standard Fortran counterpart of traits_runtime_view_argument_01: deferred
! bindings of abstract types take a polymorphic argument other than their
! passed object and modify it in place through its own bindings and SELECT
! TYPE. One abstract type stands for both ICoeffs and IReporter, since a type
! extends only one parent; the calls, values and addresses are the same.
module traits_runtime_view_argument_01_oracle_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    implicit none
    type(c_ptr) :: seen = c_null_ptr
    integer :: finals = 0

    type, abstract :: IEstimates
    contains
        procedure(some_method_i), deferred :: some_method
    end type IEstimates

    type, abstract, extends(IEstimates) :: IMeasured
    contains
        procedure(measure_i), deferred :: measure
    end type IMeasured

    type, abstract :: ICoeffs
    contains
        procedure(update_i), deferred :: update
        procedure(report_i), deferred, pass(self) :: report
        procedure(weigh_i), deferred :: weigh
        procedure(transfer_i), deferred, nopass :: transfer
    end type ICoeffs

    abstract interface
        subroutine some_method_i(self)
            import IEstimates
            class(IEstimates), target, intent(inout) :: self
        end subroutine some_method_i
        integer function measure_i(self)
            import IMeasured
            class(IMeasured), intent(in) :: self
        end function measure_i
        subroutine update_i(self, estimates)
            import ICoeffs, IEstimates
            class(ICoeffs), intent(inout) :: self
            class(IEstimates), intent(inout) :: estimates
        end subroutine update_i
        subroutine report_i(sink, self)
            import ICoeffs, IEstimates
            class(IEstimates), intent(inout) :: sink
            class(ICoeffs), intent(in) :: self
        end subroutine report_i
        integer function weigh_i(self, estimates)
            import ICoeffs, IMeasured
            class(ICoeffs), intent(in) :: self
            class(IMeasured), intent(in) :: estimates
        end function weigh_i
        subroutine transfer_i(from, to)
            import IMeasured, IEstimates
            class(IMeasured), intent(in) :: from
            class(IEstimates), intent(inout) :: to
        end subroutine transfer_i
    end interface

    type :: MyType
        private
        class(ICoeffs), allocatable :: coeffs
        class(IEstimates), allocatable :: estimates
    contains
        procedure, private :: coefficients
    end type MyType

    type, extends(IEstimates) :: Tally
        integer :: n = 0
        integer :: calls = 0
    contains
        procedure :: some_method => estimates_step
        final :: finish
    end type Tally

    type, extends(IMeasured) :: Measured
        integer :: n = 0
    contains
        procedure :: some_method => measured_step
        procedure :: measure => measured_value
    end type Measured

    type, extends(ICoeffs) :: Coeffs
        integer :: updates = 0
    contains
        procedure :: update => coeffs_update
        procedure, pass(self) :: report => coeffs_report
        procedure :: weigh => coeffs_weigh
        procedure, nopass :: transfer => coeffs_transfer
    end type Coeffs
contains
    subroutine coefficients(self)
        class(MyType), intent(inout) :: self
        call self%coeffs%update(self%estimates)
    end subroutine coefficients

    subroutine estimates_step(self)
        class(Tally), target, intent(inout) :: self
        seen = c_loc(self%n)
        self%n = self%n + 1
    end subroutine estimates_step

    subroutine measured_step(self)
        class(Measured), target, intent(inout) :: self
        seen = c_loc(self%n)
        self%n = self%n + 100
    end subroutine measured_step

    integer function measured_value(self)
        class(Measured), intent(in) :: self
        measured_value = self%n
    end function measured_value

    subroutine finish(self)
        type(Tally), intent(inout) :: self
        finals = finals + 1
    end subroutine finish

    subroutine coeffs_update(self, estimates)
        class(Coeffs), intent(inout) :: self
        class(IEstimates), intent(inout) :: estimates
        self%updates = self%updates + 1
        call estimates%some_method()
        select type (concrete => estimates)
        type is (Tally)
            concrete%calls = concrete%calls + 1
        type is (Measured)
        class default
            error stop 1
        end select
    end subroutine coeffs_update

    subroutine coeffs_report(sink, self)
        class(IEstimates), intent(inout) :: sink
        class(Coeffs), intent(in) :: self
        integer :: i
        do i = 1, self%updates
            call sink%some_method()
        end do
    end subroutine coeffs_report

    integer function coeffs_weigh(self, estimates)
        class(Coeffs), intent(in) :: self
        class(IMeasured), intent(in) :: estimates
        coeffs_weigh = 1000 * self%updates + estimates%measure()
    end function coeffs_weigh

    subroutine coeffs_transfer(from, to)
        class(IMeasured), intent(in) :: from
        class(IEstimates), intent(inout) :: to
        integer :: i
        do i = 1, from%measure() / 100
            call to%some_method()
        end do
    end subroutine coeffs_transfer

    subroutine setup(t, measured_estimates)
        type(MyType), intent(inout) :: t
        logical, intent(in) :: measured_estimates
        allocate(Coeffs :: t%coeffs)
        if (measured_estimates) then
            allocate(t%estimates, source=Measured(n=5))
        else
            allocate(Tally :: t%estimates)
        end if
    end subroutine setup

    subroutine run_coefficients(t)
        type(MyType), intent(inout) :: t
        call t%coefficients()
    end subroutine run_coefficients

    subroutine estimates_state(t, address, n, calls, updates)
        type(MyType), target, intent(in) :: t
        type(c_ptr), intent(out) :: address
        integer, intent(out) :: n, calls, updates
        calls = -1
        select type (concrete => t%estimates)
        type is (Tally)
            address = c_loc(concrete%n)
            n = concrete%n
            calls = concrete%calls
        type is (Measured)
            address = c_loc(concrete%n)
            n = concrete%n
        class default
            error stop 2
        end select
        select type (concrete => t%coeffs)
        type is (Coeffs)
            updates = concrete%updates
        class default
            error stop 3
        end select
    end subroutine estimates_state

    subroutine forward(coeffs, estimates)
        class(ICoeffs), intent(inout) :: coeffs
        class(IEstimates), intent(inout) :: estimates
        call coeffs%update(estimates)
    end subroutine forward

    subroutine forward_readonly_pointer(coeffs, estimates)
        class(ICoeffs), pointer, intent(in) :: coeffs
        class(IEstimates), pointer, intent(in) :: estimates
        call coeffs%update(estimates=estimates)
    end subroutine forward_readonly_pointer

    subroutine report_into(reporter, sink)
        class(ICoeffs), intent(in) :: reporter
        class(IEstimates), intent(inout) :: sink
        call reporter%report(sink)
    end subroutine report_into
end module traits_runtime_view_argument_01_oracle_m

program traits_runtime_view_argument_01_oracle
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    use traits_runtime_view_argument_01_oracle_m
    implicit none
    type(MyType) :: plain, weighed
    type(Coeffs), target :: direct
    type(Tally), target :: e, es(3)
    type(Measured) :: m
    class(ICoeffs), allocatable :: owner
    class(ICoeffs), pointer :: cp
    class(IEstimates), allocatable, target :: e_owner
    class(IEstimates), pointer :: ep
    class(IMeasured), allocatable :: m_owner
    class(ICoeffs), allocatable :: reporter
    type(c_ptr) :: address
    integer :: n, calls, updates, i

    call setup(plain, .false.)
    do i = 1, 3
        call run_coefficients(plain)
        call estimates_state(plain, address, n, calls, updates)
        if (.not. c_associated(seen, address)) error stop 10
        if (n /= i .or. calls /= i .or. updates /= i) error stop 11
    end do
    call setup(weighed, .true.)
    call run_coefficients(weighed)
    call estimates_state(weighed, address, n, calls, updates)
    if (.not. c_associated(seen, address)) error stop 12
    if (n /= 105 .or. calls /= -1 .or. updates /= 1) error stop 13

    allocate(Coeffs :: owner)
    call owner%update(e)
    if (e%n /= 1 .or. e%calls /= 1) error stop 20
    if (.not. c_associated(seen, c_loc(e%n))) error stop 21
    call owner%update(estimates=es(2))
    if (es(2)%n /= 1 .or. es(2)%calls /= 1 .or. es(1)%n /= 0 .or. es(3)%n /= 0) error stop 22
    if (.not. c_associated(seen, c_loc(es(2)%n))) error stop 23
    allocate(e_owner, source=m)
    call owner%update(e_owner)
    select type (concrete => e_owner)
    type is (Measured)
        if (concrete%n /= 100) error stop 24
        if (.not. c_associated(seen, c_loc(concrete%n))) error stop 25
    class default
        error stop 26
    end select
    if (m%n /= 0) error stop 27
    allocate(m_owner, source=Measured(n=7))
    call owner%update(m_owner)
    if (m_owner%measure() /= 107) error stop 28

    cp => direct
    ep => e
    call cp%update(ep)
    if (e%n /= 2 .or. e%calls /= 2 .or. direct%updates /= 1) error stop 30
    call forward_readonly_pointer(cp, ep)
    if (e%n /= 3 .or. e%calls /= 3 .or. direct%updates /= 2) error stop 31
    if (.not. c_associated(seen, c_loc(e%n))) error stop 32
    call forward(owner, e)
    if (e%n /= 4 .or. e%calls /= 4) error stop 33
    call forward(direct, m_owner)
    if (m_owner%measure() /= 207 .or. direct%updates /= 3) error stop 34

    call direct%update(e_owner)
    call direct%update(estimates=es(3))
    if (direct%updates /= 5 .or. es(3)%n /= 1 .or. es(3)%calls /= 1) error stop 40
    select type (concrete => e_owner)
    type is (Measured)
        if (concrete%n /= 200) error stop 41
    end select

    allocate(reporter, source=direct)
    call report_into(reporter, e)
    if (e%n /= 9 .or. e%calls /= 4) error stop 50
    call reporter%report(sink=es(1))
    if (es(1)%n /= 5 .or. .not. c_associated(seen, c_loc(es(1)%n))) error stop 51
    if (reporter%weigh(Measured(n=3)) /= 5003) error stop 52
    if (reporter%weigh(estimates=m_owner) /= 5207) error stop 53
    call reporter%transfer(m_owner, e)
    if (e%n /= 11) error stop 54
    call reporter%transfer(to=es(1), from=m_owner)
    if (es(1)%n /= 7) error stop 55
    if (m_owner%measure() /= 207 .or. direct%updates /= 5) error stop 56

    if (finals /= 0) error stop 60
    print '(a)', 'traits_runtime_view_argument_01_oracle ok'
end program traits_runtime_view_argument_01_oracle
