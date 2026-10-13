module traits_runtime_owning_09_m
    implicit none
    integer :: assignments = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type, abstract :: Base
    contains
        procedure(assign_signature), deferred :: assign
        generic :: assignment(=) => assign
    end type
    type, extends(Base) :: Child
        integer :: n = 10
    contains
        procedure :: assign => assign_child
    end type
    abstract interface
        subroutine assign_signature(self, other)
            import Base, Child
            class(Base), intent(inout) :: self
            type(Child), intent(in) :: other
        end subroutine
    end interface
    type :: Envelope
        type(Child) :: part
    end type
    implements IValue :: Envelope
        procedure, pass :: value => read_value
    end implements
contains
    subroutine assign_child(self, other)
        class(Child), intent(inout) :: self
        type(Child), intent(in) :: other
        self%n = self%n + other%n
        assignments = assignments + 1
    end subroutine
    function read_value(self) result(r)
        class(Envelope), intent(in) :: self
        integer :: r
        r = self%part%n
    end function
end module

program traits_runtime_owning_09
    use traits_runtime_owning_09_m
    implicit none
    type(Envelope) :: source
    class(IValue), allocatable :: owner, copy
    source%part%n = 3
    allocate(copy, source=source)
    if (assignments /= 0 .or. copy%value() /= 3) error stop 1
    allocate(Envelope :: owner)
    owner = source
    if (assignments /= 1 .or. owner%value() /= 13) error stop 2
    owner = owner
    if (assignments /= 2 .or. owner%value() /= 26) error stop 3
    if (copy%value() /= 3) error stop 4
    deallocate(owner, copy)
end program
