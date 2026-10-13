module traits_runtime_owning_10_m
    implicit none
    integer :: child_calls = 0, grandchild_calls = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type, abstract :: Base
    contains
        procedure(assign_signature), deferred, pass(other) :: assign
        generic :: assignment(=) => assign
    end type
    abstract interface
        subroutine assign_signature(self, other)
            import Base
            class(Base), intent(inout) :: self
            class(Base), intent(in) :: other
        end subroutine
    end interface
    type, extends(Base) :: Child
        integer :: n = 10
    contains
        procedure, pass(other) :: assign => assign_child
    end type
    type, extends(Child) :: Grandchild
    contains
        procedure, pass(other) :: assign => assign_grandchild
    end type
    type :: Envelope
        type(Grandchild) :: concrete
        class(Base), allocatable :: polymorphic
    end type
    implements IValue :: Envelope
        procedure, pass :: value => read_value
    end implements
contains
    subroutine assign_child(self, other)
        class(Base), intent(inout) :: self
        class(Child), intent(in) :: other
        select type(self)
        class is(Child)
            self%n = self%n + other%n
        end select
        child_calls = child_calls + 1
    end subroutine
    subroutine assign_grandchild(self, other)
        class(Base), intent(inout) :: self
        class(Grandchild), intent(in) :: other
        select type(self)
        class is(Child)
            self%n = self%n + other%n + 100
        end select
        grandchild_calls = grandchild_calls + 1
    end subroutine
    function read_value(self) result(r)
        class(Envelope), intent(in) :: self
        integer :: r
        r = self%concrete%n
        if (allocated(self%polymorphic)) then
            select type(part => self%polymorphic)
            class is(Child)
                r = r + part%n
            end select
        end if
    end function
end module

program traits_runtime_owning_10
    use traits_runtime_owning_10_m
    implicit none
    type(Envelope) :: source
    class(IValue), allocatable :: owner, copy
    source%concrete%n = 3
    allocate(Grandchild :: source%polymorphic)
    select type(part => source%polymorphic)
    type is(Grandchild)
        part%n = 4
    end select
    allocate(copy, source=source)
    if (child_calls /= 0 .or. grandchild_calls /= 0) error stop 1
    if (copy%value() /= 7) error stop 2
    allocate(Envelope :: owner)
    owner = source
    if (child_calls /= 0 .or. grandchild_calls /= 2) error stop 3
    if (owner%value() /= 227) error stop 4
    owner = owner
    if (child_calls /= 0 .or. grandchild_calls /= 4) error stop 5
    if (owner%value() /= 550) error stop 6
    if (copy%value() /= 7) error stop 7
    deallocate(owner, copy)
    deallocate(source%polymorphic)
end program
