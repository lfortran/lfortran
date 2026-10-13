module traits_runtime_projection_01_oracle_m
    implicit none
    type :: Base
        integer :: n
    contains
        procedure :: value
    end type
    type, extends(Base) :: Child
        integer :: extra
    end type
    class(Base), pointer :: saved => null()
contains
    integer function value(self)
        class(Base), intent(in) :: self
        value = self%n
    end function
    logical function empty(view)
        class(Base), pointer, intent(in) :: view
        empty = .not. associated(view)
    end function
    subroutine remember(view)
        class(Base), pointer, intent(in) :: view
        saved => view
    end subroutine
end module

program traits_runtime_projection_01_oracle
    use traits_runtime_projection_01_oracle_m
    implicit none
    type(Child), target :: source
    class(Child), pointer :: child_view => null()
    class(Base), pointer :: parent => null()
    class(Child), allocatable, target :: owner
    class(Base), allocatable, target :: copy
    if (.not. empty(child_view) .or. .not. empty(null(child_view))) error stop 1
    parent => child_view
    if (associated(parent)) error stop 2
    source%n = 7
    source%extra = 19
    child_view => source
    call remember(child_view)
    if (.not. associated(saved, child_view)) error stop 3
    nullify(child_view)
    source%n = 23
    if (saved%value() /= 23) error stop 4
    call remember(null(child_view))
    if (associated(saved)) error stop 5
    allocate(owner, source=source)
    copy = owner
    parent => copy
    child_view => owner
    if (associated(parent, child_view)) error stop 6
    call remember(owner)
    if (.not. associated(saved, owner)) error stop 7
    owner%n = 31
    if (saved%value() /= 31 .or. copy%value() /= 23) error stop 8
    nullify(parent, child_view, saved)
    deallocate(owner, copy)
end program
