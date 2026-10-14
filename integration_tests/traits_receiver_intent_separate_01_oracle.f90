! Standard-Fortran counterpart of traits_receiver_intent_separate_01: an
! abstract base type with deferred bindings whose passed objects are
! intent(in) for value, intent(inout) for bump and take and intent(out) for
! reset (passed second), two extensions with the same name in different
! modules, and the same consumer. It checks the same values, finalizations
! and addresses.
module traits_receiver_intent_separate_01_oracle_contracts_m
    implicit none
    type, abstract :: ICounter
    contains
        procedure(value_iface), deferred :: value
        procedure(bump_iface), deferred :: bump
        procedure(take_iface), deferred :: take
        procedure(reset_iface), deferred, pass(self) :: reset
    end type ICounter
    abstract interface
        integer function value_iface(self)
            import :: ICounter
            class(ICounter), intent(in) :: self
        end function value_iface
        subroutine bump_iface(self, by)
            import :: ICounter
            class(ICounter), intent(inout), target :: self
            integer, intent(in) :: by
        end subroutine bump_iface
        integer function take_iface(self)
            import :: ICounter
            class(ICounter), intent(inout) :: self
        end function take_iface
        subroutine reset_iface(seed, self)
            import :: ICounter
            integer, intent(in) :: seed
            class(ICounter), intent(out), target :: self
        end subroutine reset_iface
    end interface
end module traits_receiver_intent_separate_01_oracle_contracts_m

module traits_receiver_intent_separate_01_oracle_consumer_m
    use traits_receiver_intent_separate_01_oracle_contracts_m, only: ICounter
    implicit none
contains
    integer function observe(object)
        class(ICounter), intent(in) :: object
        observe = object%value()
    end function observe

    subroutine exercise(object, start, step, bumped, seed, restarted)
        class(ICounter), intent(inout) :: object
        integer, intent(in) :: start, step, bumped, seed, restarted
        integer :: taken
        if (observe(object) /= start) error stop 701
        call object%bump(step)
        if (observe(object) /= bumped) error stop 702
        taken = object%take()
        if (taken /= bumped .or. observe(object) /= 0) error stop 703
        call object%reset(seed=seed)
        if (observe(object) /= restarted) error stop 704
    end subroutine exercise

    subroutine choose(choice, first, second)
        integer, intent(in) :: choice
        class(ICounter), intent(inout) :: first, second
        if (choice == 0) then
            call exercise(first, 11, 3, 14, 4, 4)
        else
            call exercise(second, 30, 5, 45, 4, 8)
        end if
    end subroutine choose

    subroutine nudge(object, by)
        class(ICounter), pointer, intent(in) :: object
        integer, intent(in) :: by
        call object%bump(by=by)
    end subroutine nudge
end module traits_receiver_intent_separate_01_oracle_consumer_m

module traits_receiver_intent_separate_01_oracle_a_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use traits_receiver_intent_separate_01_oracle_contracts_m, only: ICounter
    implicit none
    integer :: a_finals = 0, a_finals_with_log = 0
    type(c_ptr) :: a_bump_seen = c_null_ptr, a_reset_seen = c_null_ptr
    type, extends(ICounter) :: Counter
        integer :: n = 0
        integer, allocatable :: log(:)
    contains
        procedure :: value => a_value
        procedure :: bump => a_bump
        procedure :: take => a_take
        procedure, pass(self) :: reset => a_reset
        final :: a_final
    end type Counter
contains
    integer function a_value(self)
        class(Counter), intent(in) :: self
        a_value = self%n
    end function a_value
    subroutine a_bump(self, by)
        class(Counter), intent(inout), target :: self
        integer, intent(in) :: by
        integer, allocatable :: grown(:)
        a_bump_seen = c_loc(self%n)
        self%n = self%n + by
        if (.not. allocated(self%log)) allocate(self%log(0))
        allocate(grown(size(self%log) + 1))
        grown(:size(self%log)) = self%log
        grown(size(grown)) = by
        call move_alloc(grown, self%log)
    end subroutine a_bump
    integer function a_take(self)
        class(Counter), intent(inout) :: self
        a_take = self%n
        self%n = 0
    end function a_take
    subroutine a_reset(seed, self)
        integer, intent(in) :: seed
        class(Counter), intent(out), target :: self
        if (self%n /= 0 .or. allocated(self%log)) error stop 711
        a_reset_seen = c_loc(self%n)
        self%n = seed
        allocate(self%log(0))
    end subroutine a_reset
    subroutine a_final(self)
        type(Counter), intent(inout) :: self
        a_finals = a_finals + 1
        if (allocated(self%log)) a_finals_with_log = a_finals_with_log + 1
    end subroutine a_final
end module traits_receiver_intent_separate_01_oracle_a_m

module traits_receiver_intent_separate_01_oracle_b_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use traits_receiver_intent_separate_01_oracle_contracts_m, only: ICounter
    implicit none
    type(c_ptr) :: b_bump_seen = c_null_ptr, b_reset_seen = c_null_ptr
    type, extends(ICounter) :: Counter
        integer :: n = 0
        integer :: scale = 2
    contains
        procedure :: value => b_value
        procedure :: bump => b_bump
        procedure :: take => b_take
        procedure, pass(self) :: reset => b_reset
    end type Counter
contains
    integer function b_value(self)
        class(Counter), intent(in) :: self
        b_value = self%scale * self%n
    end function b_value
    subroutine b_bump(self, by)
        class(Counter), intent(inout), target :: self
        integer, intent(in) :: by
        b_bump_seen = c_loc(self%n)
        self%n = self%n + by
    end subroutine b_bump
    integer function b_take(self)
        class(Counter), intent(inout) :: self
        b_take = self%scale * self%n
        self%n = 0
    end function b_take
    subroutine b_reset(seed, self)
        integer, intent(in) :: seed
        class(Counter), intent(out), target :: self
        if (self%n /= 0 .or. self%scale /= 2) error stop 721
        b_reset_seen = c_loc(self%n)
        self%n = seed
    end subroutine b_reset
end module traits_receiver_intent_separate_01_oracle_b_m

program traits_receiver_intent_separate_01_oracle
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    use traits_receiver_intent_separate_01_oracle_contracts_m, only: ICounter
    use traits_receiver_intent_separate_01_oracle_consumer_m, only: choose, nudge
    use traits_receiver_intent_separate_01_oracle_a_m, A => Counter
    use traits_receiver_intent_separate_01_oracle_b_m, B => Counter
    implicit none
    type(A), target :: first
    type(B), target :: second
    class(ICounter), pointer :: p
    type(c_ptr) :: first_address, second_address
    integer :: i
    first%n = 11
    second%n = 10
    second%scale = 3
    first_address = c_loc(first%n)
    second_address = c_loc(second%n)
    do i = 0, 1
        call choose(mod(command_argument_count() + i, 2), first, second)
    end do
    if (a_finals /= 1 .or. a_finals_with_log /= 1) error stop 801
    if (first%n /= 4 .or. .not. allocated(first%log)) error stop 802
    if (size(first%log) /= 0) error stop 803
    if (second%n /= 4 .or. second%scale /= 2) error stop 804
    if (.not. c_associated(a_reset_seen, first_address)) error stop 805
    if (.not. c_associated(b_reset_seen, second_address)) error stop 806
    p => first
    call nudge(p, 2)
    p => second
    call nudge(p, 3)
    if (first%n /= 6 .or. size(first%log) /= 1 .or. second%n /= 7) error stop 807
    if (.not. c_associated(a_bump_seen, first_address)) error stop 808
    if (.not. c_associated(b_bump_seen, second_address)) error stop 809
    if (a_finals /= 1) error stop 810
    print '(a)', 'traits_receiver_intent_separate_01 ok'
end program traits_receiver_intent_separate_01_oracle
