! An optional borrowed runtime trait view dummy, as the reported
! `class(ISomeTrait), optional, intent(in) :: obj` does: it is present when
! a concrete value, a class object, an owner or another view is passed, and
! absent when it is omitted or an absent optional view is forwarded; a
! present view dispatches, also through a defining receiver and through a
! message of a runtime view.
module traits_runtime_optional_view_01_m
    implicit none
    private
    public :: ISomeTrait, IUser, Cell, Base, Derived, User, peek, bump, twice

    abstract interface :: ISomeTrait
        integer function get(self)
            type(itself), pass, intent(in) :: self
        end function get
        subroutine set(self, v)
            type(itself), pass, intent(inout) :: self
            integer, intent(in) :: v
        end subroutine set
    end interface

    abstract interface :: IUser
        integer function use_it(self, obj)
            type(itself), pass, intent(in) :: self
            class(ISomeTrait), optional, intent(in) :: obj
        end function use_it
    end interface

    type, implements(ISomeTrait) :: Cell
        integer :: v = 7
    contains
        procedure :: get => cell_get
        procedure :: set => cell_set
    end type Cell

    type, abstract :: Base
    end type Base

    type, extends(Base), implements(ISomeTrait) :: Derived
        integer :: v = 11
    contains
        procedure :: get => derived_get
        procedure :: set => derived_set
    end type Derived

    type, implements(IUser) :: User
        integer :: offset = 100
    contains
        procedure :: use_it => user_use_it
    end type User

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

    integer function derived_get(self)
        class(Derived), intent(in) :: self
        derived_get = self%v
    end function derived_get

    subroutine derived_set(self, v)
        class(Derived), intent(inout) :: self
        integer, intent(in) :: v
        self%v = v
    end subroutine derived_set

    function peek(obj) result(n)
        class(ISomeTrait), optional, intent(in) :: obj
        integer :: n
        n = -1
        if (present(obj)) n = obj%get()
    end function peek

    integer function twice(obj)
        class(ISomeTrait), optional, intent(in) :: obj
        twice = 2 * peek(obj)
    end function twice

    subroutine bump(obj)
        class(ISomeTrait), optional, intent(inout) :: obj
        if (present(obj)) call obj%set(obj%get() + 1)
    end subroutine bump

    integer function user_use_it(self, obj)
        class(User), intent(in) :: self
        class(ISomeTrait), optional, intent(in) :: obj
        user_use_it = self%offset + peek(obj)
    end function user_use_it
end module traits_runtime_optional_view_01_m

program traits_runtime_optional_view_01
    use traits_runtime_optional_view_01_m
    implicit none
    type(Cell) :: c
    type(Derived) :: d
    type(User) :: u
    class(ISomeTrait), allocatable :: owner
    class(IUser), allocatable :: view

    if (peek() /= -1) error stop 1
    if (peek(c) /= 7) error stop 2
    if (peek(Cell(v=3)) /= 3) error stop 3
    if (peek(obj=d) /= 11) error stop 4
    if (twice() /= -2) error stop 5
    if (twice(c) /= 14) error stop 6

    call bump()
    call bump(c)
    if (c%v /= 8) error stop 7

    allocate(owner, source=Cell(v=20))
    if (peek(owner) /= 20) error stop 8
    call bump(owner)
    if (peek(owner) /= 21) error stop 9

    if (u%use_it() /= 99) error stop 10
    allocate(view, source=u)
    if (view%use_it() /= 99) error stop 11
    if (view%use_it(c) /= 108) error stop 12
    if (view%use_it(obj=owner) /= 121) error stop 13
    print *, peek(c), peek(owner)
end program traits_runtime_optional_view_01
