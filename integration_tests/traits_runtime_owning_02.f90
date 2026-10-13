! Exact concrete expressions use ordinary result storage, not an erased factory ABI.
module traits_runtime_owning_02_m
    implicit none
    integer :: evaluations = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Item
        integer :: n = 7
    end type
    implements IValue :: Item
        procedure, pass :: value => read_item
    end implements
contains
    function read_item(self) result(r)
        class(Item), intent(in) :: self
        integer :: r
        r = self%n
    end function
    function make_item(n) result(r)
        integer, intent(in) :: n
        type(Item) :: r
        evaluations = evaluations + 1
        r%n = n
    end function
    subroutine saved_lifetime(create)
        logical, intent(in) :: create
        class(IValue), allocatable, save :: saved
        if (create) then
            if (allocated(saved)) error stop 1
            saved = Item(55)
        else
            if (.not. allocated(saved)) error stop 2
            if (saved%value() /= 55) error stop 3
            deallocate(saved)
        end if
    end subroutine
end module

program traits_runtime_owning_02
    use traits_runtime_owning_02_m
    implicit none
    class(IValue) :: owner
    allocatable :: owner

    owner = Item(40)
    owner = Item(owner%value() + 2)
    if (owner%value() /= 42) error stop 4
    owner = make_item(owner%value() + 1)
    if (owner%value() /= 43 .or. evaluations /= 1) error stop 5
    deallocate(owner)
    allocate(owner, source=Item(21))
    if (owner%value() /= 21) error stop 6
    deallocate(owner)
    allocate(owner, source=make_item(23))
    if (owner%value() /= 23 .or. evaluations /= 2) error stop 7
    deallocate(owner)
    block
        class(IValue) :: scoped
        allocatable :: scoped
        allocate(Item :: scoped)
        if (scoped%value() /= 7) error stop 8
    end block
    call saved_lifetime(.true.)
    call saved_lifetime(.false.)
end program
