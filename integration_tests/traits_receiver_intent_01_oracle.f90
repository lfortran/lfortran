! Standard-Fortran counterpart of traits_receiver_intent_01.f90: an abstract
! base type whose deferred bindings take their passed objects with
! intent(in) and intent(inout), called through class(ICounter) dummies with
! intent(in) and intent(inout), owners, pointers (also an intent(in) pointer
! dummy) and direct calls. It checks the same values and addresses.
module traits_receiver_intent_01_oracle_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
    implicit none
    type(c_ptr) :: expected = c_null_ptr
    type, abstract :: ICounter
    contains
        procedure(value_iface), deferred :: value
        procedure(increment_iface), deferred :: increment
        procedure(add_iface), deferred :: add
    end type
    abstract interface
        integer function value_iface(self)
            import :: ICounter
            class(ICounter), intent(in) :: self
        end function
        subroutine increment_iface(self)
            import :: ICounter
            class(ICounter), target, intent(inout) :: self
        end subroutine
        subroutine add_iface(self, by)
            import :: ICounter
            class(ICounter), target, intent(inout) :: self
            integer, intent(in) :: by
        end subroutine
    end interface
    type, extends(ICounter) :: Counter
        integer :: n = 0
    contains
        procedure :: value => counter_value
        procedure :: increment => counter_increment
        procedure :: add => counter_add
    end type
    type :: Holder
        type(Counter) :: c
        class(ICounter), allocatable :: owned
    end type
contains
    integer function counter_value(self)
        class(Counter), intent(in) :: self
        counter_value = self%n
    end function
    subroutine counter_increment(self)
        class(Counter), target, intent(inout) :: self
        type(c_ptr) :: actual
        actual = c_loc(self%n)
        if (c_associated(expected)) then
            if (.not. c_associated(actual, expected)) error stop 90
        end if
        self%n = self%n + 1
    end subroutine
    subroutine counter_add(self, by)
        class(Counter), target, intent(inout) :: self
        integer, intent(in) :: by
        type(c_ptr) :: actual
        actual = c_loc(self%n)
        if (c_associated(expected)) then
            if (.not. c_associated(actual, expected)) error stop 91
        end if
        self%n = self%n + by
    end subroutine
    integer function observe(object)
        class(ICounter), intent(in) :: object
        observe = object%value()
    end function
    subroutine bump(object)
        class(ICounter), intent(inout) :: object
        call object%increment()
    end subroutine
    subroutine bump_and_add(object, by)
        class(ICounter), intent(inout) :: object
        integer, intent(in) :: by
        call bump(object)
        call object%increment()
        call object%add(by=by)
    end subroutine
    subroutine bump_target(p)
        class(ICounter), pointer, intent(in) :: p
        call p%increment()
    end subroutine
    subroutine bump_generic(x, by)
        class(ICounter), intent(inout) :: x
        integer, intent(in) :: by
        call x%increment()
        call x%add(by)
    end subroutine
    integer function read_generic(x)
        class(ICounter), intent(in) :: x
        read_generic = x%value()
    end function
end module

program traits_receiver_intent_01_oracle
    use iso_c_binding, only: c_loc, c_null_ptr
    use traits_receiver_intent_01_oracle_m
    implicit none
    type(Counter), target :: c, cells(3)
    type(Holder), target :: h
    class(ICounter), allocatable, target :: owner
    class(ICounter), pointer :: p

    expected = c_loc(c%n)
    call bump(c)
    if (c%n /= 1) error stop 1
    if (observe(c) /= 1) error stop 2
    expected = c_loc(h%c%n)
    call bump(h%c)
    if (h%c%n /= 1) error stop 3
    expected = c_loc(cells(2)%n)
    call bump_and_add(cells(2), 5)
    if (cells(1)%n /= 0 .or. cells(2)%n /= 7 .or. cells(3)%n /= 0) error stop 4
    allocate(Counter :: owner)
    select type (concrete => owner)
    type is (Counter)
        expected = c_loc(concrete%n)
    class default
        error stop 5
    end select
    call bump_and_add(owner, 10)
    if (owner%value() /= 12 .or. observe(owner) /= 12) error stop 6
    call owner%add(by=3)
    if (owner%value() /= 15) error stop 7
    allocate(Counter :: h%owned)
    select type (concrete => h%owned)
    type is (Counter)
        expected = c_loc(concrete%n)
    class default
        error stop 8
    end select
    call bump(h%owned)
    if (observe(h%owned) /= 1) error stop 9
    p => c
    expected = c_loc(c%n)
    call bump_target(p)
    call p%add(2)
    if (c%n /= 4 .or. .not. associated(p)) error stop 10
    call c%increment()
    call c%add(by=10)
    if (c%value() /= 15) error stop 11
    call bump_generic(c, 100)
    if (read_generic(c) /= 116) error stop 12
    expected = c_null_ptr
    call bump_generic(cells(1), -1)
    if (cells(1)%n /= 0 .or. read_generic(cells(2)) /= 7) error stop 13
    deallocate(owner, h%owned)
    print '(a)', 'traits_receiver_intent_01 ok'
end program
