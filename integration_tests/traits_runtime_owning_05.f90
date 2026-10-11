! A trait value does not invent a whole-object defined-assignment overload.
module traits_runtime_owning_05_m
    implicit none
    integer :: defined_calls = 0
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
    abstract interface
        subroutine assign_signature(self, other)
            import :: Base
            class(Base), intent(out) :: self
            class(Base), intent(in) :: other
        end subroutine
    end interface
    type, extends(Base) :: Concrete
        integer :: n = 0
    contains
        procedure :: assign => assign_concrete
    end type
    implements IValue :: Concrete
        procedure, pass :: value => read_value
    end implements
contains
    subroutine assign_concrete(self, other)
        class(Concrete), intent(out) :: self
        class(Base), intent(in) :: other
        select type(other)
        type is (Concrete)
            self%n = other%n + 10
        end select
        defined_calls = defined_calls + 1
    end subroutine
    function read_value(self) result(r)
        class(Concrete), intent(in) :: self
        integer :: r
        r = self%n
    end function
end module
program traits_runtime_owning_05
    use traits_runtime_owning_05_m
    implicit none
    type(Concrete) :: source
    class(IValue), allocatable :: owner, copy
    class(Base), allocatable :: ordinary
    source%n = 5
    allocate(owner, source=source)
    copy = owner
    owner = source
    if (owner%value() /= 5 .or. copy%value() /= 5) error stop 1
    if (defined_calls /= 0) error stop 2
    allocate(Concrete :: ordinary)
    ordinary = source
    select type(ordinary)
    type is (Concrete)
        if (ordinary%n /= 15) error stop 3
    end select
    if (defined_calls /= 1) error stop 4
    deallocate(owner, copy, ordinary)
end program
