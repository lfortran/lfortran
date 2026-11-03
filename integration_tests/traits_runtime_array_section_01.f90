! Sections of runtime trait arrays (TD-46) are passed to trait array dummies,
! as the reported `call process(objs(1:1))` of an allocatable
! `class(ITrait) :: objs(:)` does: a section is a descriptor of the
! selected elements in place, never a copy, so that strided, reversed,
! empty, rank-reducing and rank-2 sections, sections with nonunit lower
! bounds, of a component, of a forwarded dummy and through a dispatched
! message read the selected elements, and an intent(inout) dummy defines
! the caller's own elements.
module traits_runtime_array_section_01_traits
    implicit none
    private
    public :: ITrait, IValue, IClient

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

    abstract interface :: IClient
        integer function user(self, arr)
            import :: IValue
            type(itself), pass, intent(in) :: self
            class(IValue), intent(in) :: arr(:)
        end function user
    end interface
end module traits_runtime_array_section_01_traits

module traits_runtime_array_section_01_impl
    use traits_runtime_array_section_01_traits, only: ITrait, IValue, IClient
    implicit none
    private
    public :: MyType, Cell, Holder, Summer, process, total, total2, forward, zero

    type, implements(ITrait) :: MyType
    end type MyType

    ! An element with an allocatable component.
    type, implements(IValue) :: Cell
        integer :: v = 0
        integer, allocatable :: extra(:)
    contains
        procedure :: get => cell_get
        procedure :: set => cell_set
    end type Cell

    type :: Holder
        class(IValue), allocatable :: arr(:)
    end type Holder

    type, implements(IClient) :: Summer
    contains
        procedure :: user => summer_user
    end type Summer

    integer, public :: processed = 0

contains

    subroutine process(obj)
        class(ITrait), intent(in) :: obj(:)
        processed = processed + size(obj)
    end subroutine process

    integer function cell_get(self)
        class(Cell), intent(in) :: self
        cell_get = self%v
    end function cell_get

    subroutine cell_set(self, v)
        class(Cell), intent(inout) :: self
        integer, intent(in) :: v
        self%v = v
        if (allocated(self%extra)) deallocate(self%extra)
        allocate(self%extra(v))
    end subroutine cell_set

    integer function summer_user(self, arr)
        class(Summer), intent(in) :: self
        class(IValue), intent(in) :: arr(:)
        summer_user = total(arr)
    end function summer_user

    ! The elements as the digits of a number, in array element order.
    integer function total(a) result(s)
        class(IValue), intent(in) :: a(:)
        integer :: i
        s = 0
        do i = 1, size(a)
            s = s * 10 + a(i)%get()
        end do
    end function total

    integer function total2(a) result(s)
        class(IValue), intent(in) :: a(:, :)
        integer :: i, j
        s = 0
        do j = 1, size(a, 2)
            do i = 1, size(a, 1)
                s = s * 10 + a(i, j)%get()
            end do
        end do
    end function total2

    integer function forward(a) result(s)
        class(IValue), intent(in) :: a(:)
        s = total(a(2:))
    end function forward

    subroutine zero(a)
        class(IValue), intent(inout) :: a(:)
        integer :: i
        do i = 1, size(a)
            call a(i)%set(0)
        end do
    end subroutine zero
end module traits_runtime_array_section_01_impl

program traits_runtime_array_section_01
    use traits_runtime_array_section_01_traits, only: ITrait, IValue, IClient
    use traits_runtime_array_section_01_impl
    implicit none
    class(ITrait), allocatable :: objs(:)
    class(IValue), allocatable :: v(:), m(:, :), lb(:)
    class(IClient), allocatable :: c
    type(Holder) :: h
    integer :: i, j

    allocate(MyType :: objs(2))
    call process(objs(1:1))
    call process(objs(2:1))
    call process(objs(2:1:-1))
    if (processed /= 3) error stop 1

    allocate(Cell :: v(5))
    allocate(Cell :: m(3, 2))
    allocate(Cell :: lb(0:4))
    allocate(Cell :: h%arr(4))
    do i = 1, 5
        call v(i)%set(i)
        call lb(i - 1)%set(i)
    end do
    do j = 1, 2
        do i = 1, 3
            call m(i, j)%set(i + 3 * (j - 1))
        end do
    end do
    do i = 1, 4
        call h%arr(i)%set(10 - i)
    end do

    if (total(v(1:1)) /= 1) error stop 2
    if (total(v(2:4)) /= 234) error stop 3
    if (total(v(1:5:2)) /= 135) error stop 4
    if (total(v(5:1:-2)) /= 531) error stop 5
    if (total(v(4:3)) /= 0 .or. size(v(4:3)) /= 0) error stop 6
    if (forward(v) /= 2345) error stop 7
    if (forward(v(1:5:2)) /= 35) error stop 8
    if (total(m(2, :)) /= 25) error stop 9
    if (total(m(:, 2)) /= 456) error stop 10
    if (total2(m(2:3, :)) /= 2356) error stop 11
    if (total2(m(3:1:-2, 2:1:-1)) /= 6431) error stop 12
    if (total(lb(1:3)) /= 234) error stop 13
    if (total(lb(:)) /= 12345) error stop 14
    if (total(h%arr(2:3)) /= 87) error stop 15

    c = Summer()
    if (c%user(v(2:4)) /= 234) error stop 16
    if (c%user(v(4:1:-3)) /= 41) error stop 17

    call zero(v(2:5:3))
    if (total(v) /= 10340) error stop 18
    call zero(h%arr(::3))
    if (total(h%arr) /= 870) error stop 19
    call zero(m(1, :))
    if (total2(m) /= 23056) error stop 20
    print *, "ok"
end program traits_runtime_array_section_01
