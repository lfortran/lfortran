! Runtime messages with trait-object arguments that are not their receivers.
! As reported, ICoeffs%update takes the IEstimates object it updates as an
! ordinary intent(inout) argument beside its own intent(inout) receiver, and
! the private MyType%coefficients borrows its allocated estimates component
! for it. Through class(ICoeffs) and class(IReporter) owners, views and
! pointers, a concrete variable, an array element, owners, an owner component,
! pointer targets (also through intent(in) pointers), a forwarded view and a
! child-trait owner weakened to IEstimates are each borrowed at their original
! payload address with their own selected implementation and modified in
! place, by position or keyword, with named non-first PASS and NOPASS; nothing
! is copied or finalized. A read-only receiver may update a mutable argument,
! a read-only argument accepts a structure constructor, and a concrete
! receiver binds the same arguments statically.
module traits_runtime_view_argument_01_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    implicit none
    type(c_ptr) :: seen = c_null_ptr
    integer :: finals = 0

    abstract interface :: IEstimates
        subroutine some_method(self)
            type(itself), pass, intent(inout) :: self
        end subroutine some_method
    end interface

    abstract interface, extends(IEstimates) :: IMeasured
        integer function measure(self)
            type(itself), pass, intent(in) :: self
        end function measure
    end interface

    abstract interface :: ICoeffs
        subroutine update(self,estimates)
            type(itself), pass, intent(inout) :: self
            class(IEstimates),  intent(inout) :: estimates
        end subroutine update
    end interface

    abstract interface :: IReporter
        subroutine report(sink, self)
            class(IEstimates), intent(inout) :: sink
            type(itself), pass, intent(in) :: self
        end subroutine report
        integer function weigh(self, estimates)
            type(itself), pass, intent(in) :: self
            class(IMeasured), intent(in) :: estimates
        end function weigh
        subroutine transfer(from, to)
            class(IMeasured), intent(in) :: from
            class(IEstimates), intent(inout) :: to
        end subroutine transfer
    end interface

    type :: MyType
        private
        class(ICoeffs),    allocatable :: coeffs
        class(IEstimates), allocatable :: estimates
    contains
        procedure, private :: coefficients
    end type MyType

    type :: Tally
        integer :: n = 0
        integer :: calls = 0
    contains
        final :: finish
    end type Tally
    implements IEstimates :: Tally
        procedure, pass :: some_method => estimates_step
    end implements

    type :: Measured
        integer :: n = 0
    end type Measured
    implements IMeasured :: Measured
        procedure, pass :: some_method => measured_step
        procedure, pass :: measure => measured_value
    end implements

    type :: Coeffs
        integer :: updates = 0
    end type Coeffs
    implements ICoeffs + IReporter :: Coeffs
        procedure, pass :: update => coeffs_update
        procedure, pass(me) :: report => coeffs_report
        procedure, pass :: weigh => coeffs_weigh
        procedure, nopass :: transfer => coeffs_transfer
    end implements
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

    ! The argument is modified through a message with an intent(inout)
    ! receiver and through concrete SELECT TYPE inspection.
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

    subroutine coeffs_report(sink, me)
        class(IEstimates), intent(inout) :: sink
        class(Coeffs), intent(in) :: me
        integer :: i
        do i = 1, me%updates
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

    ! Test helpers: allocate both private components, call the private
    ! binding, and inspect the components.
    subroutine setup(t, measured_estimates)
        type(MyType), intent(inout) :: t
        logical, intent(in) :: measured_estimates
        allocate(Coeffs :: t%coeffs)
        if (measured_estimates) then
            allocate(t%estimates, source=Measured(5))
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

    ! An intent(in) pointer protects its association, not its target.
    subroutine forward_readonly_pointer(coeffs, estimates)
        class(ICoeffs), pointer, intent(in) :: coeffs
        class(IEstimates), pointer, intent(in) :: estimates
        call coeffs%update(estimates=estimates)
    end subroutine forward_readonly_pointer

    ! A read-only receiver updates a mutable argument.
    subroutine report_into(reporter, sink)
        class(IReporter), intent(in) :: reporter
        class(IEstimates), intent(inout) :: sink
        call reporter%report(sink)
    end subroutine report_into
end module traits_runtime_view_argument_01_m

program traits_runtime_view_argument_01
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    use traits_runtime_view_argument_01_m
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
    class(IReporter), allocatable :: reporter
    type(c_ptr) :: address
    integer :: n, calls, updates, i

    ! The reported path, through public helpers of MyType.
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

    ! Through an owner: a concrete variable, an array element by keyword, an
    ! owner of another implementation and a child-trait owner.
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
    allocate(m_owner, source=Measured(7))
    call owner%update(m_owner)
    if (m_owner%measure() /= 107) error stop 28

    ! Through a pointer to a concrete receiver: a pointer target, also through
    ! intent(in) pointers; a forwarded view and a forwarded receiver.
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

    ! A concrete receiver binds the message statically.
    call direct%update(e_owner)
    call direct%update(estimates=es(3))
    if (direct%updates /= 5 .or. es(3)%n /= 1 .or. es(3)%calls /= 1) error stop 40
    select type (concrete => e_owner)
    type is (Measured)
        if (concrete%n /= 200) error stop 41
    end select

    ! A read-only receiver with a mutable argument (named non-first PASS), a
    ! read-only argument of a function, and a receiverless message.
    allocate(reporter, source=direct)
    call report_into(reporter, e)
    if (e%n /= 9 .or. e%calls /= 4) error stop 50
    call reporter%report(sink=es(1))
    if (es(1)%n /= 5 .or. .not. c_associated(seen, c_loc(es(1)%n))) error stop 51
    if (reporter%weigh(Measured(3)) /= 5003) error stop 52
    if (reporter%weigh(estimates=m_owner) /= 5207) error stop 53
    call reporter%transfer(m_owner, e)
    if (e%n /= 11) error stop 54
    call reporter%transfer(to=es(1), from=m_owner)
    if (es(1)%n /= 7) error stop 55
    if (m_owner%measure() /= 207 .or. direct%updates /= 5) error stop 56

    ! Borrowing copied and finalized nothing.
    if (finals /= 0) error stop 60
    print '(a)', 'traits_runtime_view_argument_01 ok'
end program traits_runtime_view_argument_01
