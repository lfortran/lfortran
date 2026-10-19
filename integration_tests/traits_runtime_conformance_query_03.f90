! The associate name of a trait guard borrows the payload of its selector
! (TD-11): messages with intent(inout) receivers modify the original object
! when the selector is definable, and nothing is copied or finalized.
module traits_runtime_conformance_query_03_m
    implicit none
    abstract interface :: ICounter
        pure integer function get(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine bump(self, by)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: by
        end subroutine
    end interface
    abstract interface :: IOther
        integer function other(self)
            type(itself), intent(in), pass :: self
        end function
    end interface
    type :: Base
        integer :: n = 0
    end type
    type, extends(Base), implements(ICounter) :: Cell
    contains
        procedure :: get => cell_get
        procedure :: bump => cell_bump
        final :: cell_final
    end type
    type, implements(ICounter + IOther) :: Both
        integer :: n = 0
    contains
        procedure :: get => both_get
        procedure :: bump => both_bump
        procedure :: other => both_other
    end type
    type :: Holder
        class(*), allocatable :: any
    end type
    integer :: finals = 0
contains
    pure integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%n
    end function
    subroutine cell_bump(self, by)
        class(Cell), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + by
    end subroutine
    subroutine cell_final(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
    end subroutine
    pure integer function both_get(self)
        class(Both), intent(in) :: self
        both_get = self%n
    end function
    subroutine both_bump(self, by)
        class(Both), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + 2 * by
    end subroutine
    integer function both_other(self)
        class(Both), intent(in) :: self
        both_other = -self%n
    end function

    subroutine bump_any(object, by)
        class(*), intent(inout) :: object
        integer, intent(in) :: by
        select type (counter => object)
        class is (ICounter)
            call counter%bump(by)
        end select
    end subroutine
    subroutine bump_pointer(object, by)
        class(*), pointer, intent(in) :: object
        integer, intent(in) :: by
        select type (counter => object)
        class is (ICounter)
            call counter%bump(by)
        end select
    end subroutine
    subroutine bump_base(object, by)
        class(Base), intent(inout) :: object
        integer, intent(in) :: by
        select type (object)
        class is (ICounter)
            call object%bump(by)
        end select
    end subroutine
    subroutine bump_view(view, by)
        class(IOther), intent(inout) :: view
        integer, intent(in) :: by
        select type (counter => view)
        class is (ICounter)
            call counter%bump(by)
            if (counter%get() /= -view%other()) error stop 1
        end select
    end subroutine
    ! A readonly selector still reads through intent(in) receivers.
    integer function read_any(object) result(r)
        class(*), intent(in) :: object
        r = -1
        select type (counter => object)
        class is (ICounter)
            r = counter%get()
        end select
    end function
    ! Nested inspection of the view defines the same payload.
    subroutine reset_nested(object)
        class(*), intent(inout) :: object
        select type (counter => object)
        class is (ICounter)
            select type (concrete => counter)
            type is (Cell)
                concrete%n = 0
            end select
        end select
    end subroutine
    pure integer function pure_read(object) result(r)
        class(*), intent(in) :: object
        r = -1
        select type (counter => object)
        class is (ICounter)
            r = counter%get()
        end select
    end function
end module

program traits_runtime_conformance_query_03
    use traits_runtime_conformance_query_03_m
    implicit none
    type(Cell), target :: a
    type(Both), target :: b
    class(*), pointer :: p
    class(ICounter), allocatable :: owner
    class(IOther), pointer :: view
    type(Holder) :: h

    call bump_any(a, 1)
    if (a%n /= 1) error stop 2
    p => a
    call bump_pointer(p, 10)
    if (a%n /= 11 .or. read_any(p) /= 11) error stop 3
    call bump_base(a, 100)
    if (a%n /= 111 .or. pure_read(a) /= 111) error stop 4
    call bump_any(b, 1)
    if (b%n /= 2) error stop 5
    view => b
    call bump_view(view, 5)
    if (b%n /= 12 .or. read_any(b) /= 12) error stop 6
    call reset_nested(a)
    if (a%n /= 0 .or. finals /= 0) error stop 7

    allocate(owner, source=a)
    select type (counter => owner)
    class is (IOther)
        error stop 8
    class is (ICounter)
        call counter%bump(7)
    end select
    if (owner%get() /= 7 .or. a%n /= 0) error stop 9
    deallocate(owner)
    if (finals /= 1) error stop 10

    allocate(h%any, source=b)
    select type (counter => h%any)
    class is (ICounter)
        call counter%bump(3)
    end select
    if (read_any(h%any) /= 18 .or. b%n /= 12) error stop 11
    if (finals /= 1) error stop 12
    print *, "trait guards: borrowed definable and readonly views"
end program
