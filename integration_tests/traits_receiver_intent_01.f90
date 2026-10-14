! LFortran traits extension test; GFortran does not accept this syntax.
! Explicit receivers (TD-36): ICounter declares a getter with an intent(in)
! receiver and two mutators with intent(inout) receivers. A read-only view of
! a Counter calls the getter even though the trait also has mutators, and an
! intent(inout) view calls the mutators on the original object, at its
! original address: a concrete variable, a component, an array element, an
! allocated owner, an owner component, a pointer target (also through an
! intent(in) pointer dummy, which protects only the association) and a
! forwarded view. Keyword arguments name the ordinary dummies only. Static
! generic calls through a constrained type parameter and direct calls mutate
! the same way. traits_receiver_intent_01_oracle.f90 is the standard-Fortran
! counterpart.
module traits_receiver_intent_01_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated, c_null_ptr
    implicit none
    type(c_ptr) :: expected = c_null_ptr
    abstract interface :: ICounter
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine increment(self)
            type(itself), intent(inout), pass :: self
        end subroutine
        subroutine add(self, by)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: by
        end subroutine
    end interface
    type :: Counter
        integer :: n = 0
    end type
    implements ICounter :: Counter
        procedure, pass :: value => counter_value
        procedure, pass :: increment => counter_increment
        procedure, pass :: add => counter_add
    end implements
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
    subroutine bump_generic{ICounter :: T}(x, by)
        type(T), intent(inout) :: x
        integer, intent(in) :: by
        call x%increment()
        call x%add(by)
    end subroutine
    integer function read_generic{ICounter :: T}(x)
        type(T), intent(in) :: x
        read_generic = x%value()
    end function
end module

program traits_receiver_intent_01
    use iso_c_binding, only: c_loc, c_null_ptr
    use traits_receiver_intent_01_m
    implicit none
    type(Counter), target :: c, cells(3)
    type(Holder), target :: h
    class(ICounter), allocatable, target :: owner
    class(ICounter), pointer :: p

    ! A Counter passed to an intent(inout) view is incremented in place.
    expected = c_loc(c%n)
    call bump(c)
    if (c%n /= 1) error stop 1
    ! A read-only view calls the getter of a trait that also has mutators.
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
    ! Direct calls and static generic calls on concrete objects.
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
