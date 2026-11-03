! An ASSOCIATE name of a runtime trait object, as the reported
! `associate(obj => objs(2))` of an element of an allocatable
! `class(ITrait) :: objs(:)`: the name is a borrowed view of the selector's
! payload with its witness, for an element of an owner or of a component,
! a scalar owner, a component, a view dummy and another associate name. Its
! messages dispatch, a defining one updates the selector, and it is passed
! to a view dummy, as the selector would be.
module traits_runtime_associate_01_traits
    implicit none
    private
    public :: ITrait, IValue

    abstract interface :: ITrait
    end interface

    abstract interface :: IValue
        integer function get(self)
            type(itself), pass, intent(in) :: self
        end function get
        subroutine set(self, v)
            type(itself), pass, intent(inout) :: self
            integer, intent(in) :: v
        end subroutine set
    end interface
end module traits_runtime_associate_01_traits

module traits_runtime_associate_01_impl
    use traits_runtime_associate_01_traits, only: ITrait, IValue
    implicit none
    private
    public :: MyType, Cell, Big, Holder, twice, peek

    type, implements(ITrait) :: MyType
    end type MyType

    type, implements(IValue) :: Cell
        integer :: v = 0
    contains
        procedure :: get => cell_get
        procedure :: set => cell_set
    end type Cell

    ! A larger element with an allocatable component.
    type, implements(IValue) :: Big
        real(8) :: pad(4) = 0
        integer, allocatable :: v(:)
    contains
        procedure :: get => big_get
        procedure :: set => big_set
    end type Big

    type :: Holder
        class(IValue), allocatable :: one
        class(IValue), allocatable :: many(:)
    end type Holder

contains

    integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%v
    end function cell_get

    subroutine cell_set(self, v)
        class(Cell), intent(inout) :: self
        integer, intent(in) :: v
        self%v = v
    end subroutine cell_set

    integer function big_get(self)
        class(Big), intent(in) :: self
        big_get = -1
        if (allocated(self%v)) big_get = self%v(1)
    end function big_get

    subroutine big_set(self, v)
        class(Big), intent(inout) :: self
        integer, intent(in) :: v
        if (allocated(self%v)) deallocate(self%v)
        allocate(self%v(1))
        self%v(1) = v
    end subroutine big_set

    subroutine twice(x)
        class(IValue), intent(inout) :: x
        call x%set(2 * x%get())
    end subroutine twice

    integer function peek(x)
        class(IValue), intent(in) :: x
        peek = x%get()
    end function peek
end module traits_runtime_associate_01_impl

program traits_runtime_associate_01
    use traits_runtime_associate_01_traits, only: ITrait, IValue
    use traits_runtime_associate_01_impl
    implicit none
    class(ITrait), allocatable :: objs(:)
    class(IValue), allocatable :: vals(:), single
    type(Holder) :: h
    integer :: i

    allocate(MyType :: objs(2))
    associate (obj => objs(2))
    end associate

    allocate(Big :: vals(3))
    do i = 1, 3
        call vals(i)%set(10 * i)
    end do
    associate (v => vals(2))
        if (v%get() /= 20) error stop 1
        call v%set(25)
        if (vals(2)%get() /= 25) error stop 2
        call twice(v)
        if (vals(2)%get() /= 50 .or. peek(v) /= 50) error stop 3
        associate (w => v)
            call w%set(7)
        end associate
    end associate
    if (vals(2)%get() /= 7) error stop 4
    if (vals(1)%get() /= 10 .or. vals(3)%get() /= 30) error stop 5

    allocate(single, source=Cell(3))
    associate (s => single, e => vals(3))
        call s%set(s%get() + e%get())
    end associate
    if (single%get() /= 33) error stop 6

    allocate(Cell :: h%one)
    allocate(Cell :: h%many(2))
    associate (a => h%one, b => h%many(2))
        call a%set(4)
        call b%set(5)
    end associate
    if (h%one%get() /= 4 .or. h%many(2)%get() /= 5 .or. h%many(1)%get() /= 0) error stop 7

    call through(single)
    if (single%get() /= 66) error stop 8
    print *, "ok"

contains

    subroutine through(x)
        class(IValue), intent(inout) :: x
        associate (y => x)
            call twice(y)
        end associate
    end subroutine through
end program traits_runtime_associate_01
