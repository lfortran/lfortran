! A derived type with a runtime trait component, declared in a module other
! than its trait, is allocated with a type specification, as the reported
! `allocate(MyType :: arr(1))` of a `class(AbsType), allocatable,
! intent(out) :: arr(:)` dummy followed by `arr = MyType(self%obj)` does:
! the polymorphic array and scalar receive copies of the component.
module traits_runtime_component_allocate_01_trait
    implicit none
    private
    public :: ISomeTrait, Cell

    abstract interface :: ISomeTrait
        integer function get(self)
            type(itself), pass, intent(in) :: self
        end function get
    end interface

    type, implements(ISomeTrait) :: Cell
        integer :: v = 0
    contains
        procedure :: get => cell_get
    end type Cell

contains

    integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%v
    end function cell_get
end module traits_runtime_component_allocate_01_trait

module traits_runtime_component_allocate_01_client
    use traits_runtime_component_allocate_01_trait
    implicit none
    private
    public :: AbsType, MyType, Client, held

    type, abstract :: AbsType
    end type AbsType

    type, extends(AbsType) :: MyType
        class(ISomeTrait), allocatable :: obj
    end type MyType

    type :: Client
        class(ISomeTrait), allocatable :: obj
    contains
        procedure :: method
        procedure :: single
    end type Client

contains

    subroutine method(self, arr)
        class(Client), intent(in) :: self
        class(AbsType), allocatable, intent(out) :: arr(:)
        allocate(MyType :: arr(1))
        arr = MyType(self%obj)
    end subroutine method

    subroutine single(self, x)
        class(Client), intent(in) :: self
        class(AbsType), allocatable, intent(out) :: x
        allocate(MyType :: x)
        select type (x)
        type is (MyType)
            x = MyType(self%obj)
        end select
    end subroutine single

    integer function held(x)
        class(AbsType), intent(in) :: x
        held = -1
        select type (x)
        type is (MyType)
            if (allocated(x%obj)) held = x%obj%get()
        end select
    end function held
end module traits_runtime_component_allocate_01_client

program traits_runtime_component_allocate_01
    use traits_runtime_component_allocate_01_trait
    use traits_runtime_component_allocate_01_client
    implicit none
    type(Client) :: c
    class(AbsType), allocatable :: arr(:), x

    allocate(c%obj, source=Cell(v=5))
    call c%method(arr)
    if (size(arr) /= 1) error stop 1
    if (held(arr(1)) /= 5) error stop 2
    call c%single(x)
    if (held(x) /= 5) error stop 3
    print *, held(arr(1)), held(x)
end program traits_runtime_component_allocate_01
