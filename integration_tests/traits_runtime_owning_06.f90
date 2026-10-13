module traits_runtime_owning_06_m
    implicit none
    integer :: parent_finals = 0, child_finals = 0, final_order = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Parent
        integer :: n = 17
    contains
        final :: finish_parent
    end type
    type, extends(Parent) :: Child
        integer :: extra = 3
    contains
        final :: finish_child
    end type
    implements IValue :: Child
        procedure, pass :: value => read_value
    end implements
contains
    subroutine finish_parent(self)
        type(Parent), intent(inout) :: self
        if (self%n /= 17) error stop 1
        parent_finals = parent_finals + 1
        final_order = 10 * final_order + 2
    end subroutine
    subroutine finish_child(self)
        type(Child), intent(inout) :: self
        if (self%n + self%extra /= 20) error stop 2
        child_finals = child_finals + 1
        final_order = 10 * final_order + 1
    end subroutine
    function read_value(self) result(r)
        class(Child), intent(in) :: self
        integer :: r
        r = self%n + self%extra
    end function
end module

program traits_runtime_owning_06
    use traits_runtime_owning_06_m
    implicit none
    class(IValue), allocatable :: owner, copy
    type(Child) :: source

    allocate(owner, source=source)
    copy = owner
    if (parent_finals /= 0 .or. child_finals /= 0) error stop 3
    owner = owner
    if (parent_finals /= 1 .or. child_finals /= 1) error stop 4
    if (owner%value() /= 20 .or. copy%value() /= 20) error stop 5
    deallocate(owner)
    if (parent_finals /= 2 .or. child_finals /= 2) error stop 6
    if (copy%value() /= 20) error stop 7
    deallocate(copy)
    if (parent_finals /= 3 .or. child_finals /= 3) error stop 8
    if (final_order /= 121212) error stop 9
end program
