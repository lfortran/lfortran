! An allocatable runtime trait array (TD-46) is a component, as the reported
! `class(ITrait), allocatable :: arr(:)` of `type :: MyType` is: it is
! allocated with a type specification or a SOURCE, also in an element of an
! array of structures, a nested structure, an extension, an allocatable
! structure and a module variable, its elements dispatch, it is passed to
! borrowed and allocatable trait array dummies, and intrinsic assignment of
! the structure, also to itself, copies its elements independently. Its
! elements are finalized when it is deallocated, when its structure is an
! intent(out) dummy and when an assignment replaces them.
module traits_runtime_array_component_01_mod
    implicit none
    integer :: finals = 0

    abstract interface :: ITrait
        integer function get(self)
            type(itself), pass, intent(in) :: self
        end function get
        subroutine set(self, v)
            type(itself), pass, intent(inout) :: self
            integer, intent(in) :: v
        end subroutine set
    end interface

    type, implements(ITrait) :: Cell
        integer :: v = 7
    contains
        procedure :: get => cell_get
        procedure :: set => cell_set
        final :: cell_final
    end type Cell

    ! A larger element with an allocatable component.
    type, implements(ITrait) :: Wide
        real(8) :: pad(3) = 0
        integer, allocatable :: v(:)
    contains
        procedure :: get => wide_get
        procedure :: set => wide_set
    end type Wide

    type :: MyType
        integer :: tag = 0
        class(ITrait), allocatable :: arr(:)
    end type MyType

    type, extends(MyType) :: Child
        integer :: extra = 1
    end type Child

    type :: Outer
        type(MyType) :: inner
        class(ITrait), allocatable :: grid(:, :)
    end type Outer

    type(MyType) :: global

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

    subroutine cell_final(self)
        type(Cell), intent(inout) :: self
        finals = finals + 1
    end subroutine cell_final

    integer function wide_get(self)
        class(Wide), intent(in) :: self
        wide_get = -1
        if (allocated(self%v)) wide_get = sum(self%v) + int(sum(self%pad))
    end function wide_get

    subroutine wide_set(self, v)
        class(Wide), intent(inout) :: self
        integer, intent(in) :: v
        if (allocated(self%v)) deallocate(self%v)
        allocate(self%v(2))
        self%v = [v, 0]
    end subroutine wide_set

    subroutine fill(arr)
        class(ITrait), intent(inout) :: arr(:)
        integer :: i
        do i = 1, size(arr)
            call arr(i)%set(10 * i)
        end do
    end subroutine fill

    integer function total(arr) result(s)
        class(ITrait), intent(in) :: arr(:)
        integer :: i
        s = 0
        do i = 1, size(arr)
            s = s + arr(i)%get()
        end do
    end function total

    subroutine make(arr, n)
        class(ITrait), allocatable, intent(out) :: arr(:)
        integer, intent(in) :: n
        allocate(Wide :: arr(n))
        call fill(arr)
    end subroutine make

    integer function holder_total(h) result(s)
        type(MyType), intent(in) :: h
        s = total(h%arr)
    end function holder_total

    subroutine reset(h)
        type(MyType), intent(out) :: h
        if (allocated(h%arr)) error stop 90
    end subroutine reset
end module traits_runtime_array_component_01_mod

program traits_runtime_array_component_01
    use traits_runtime_array_component_01_mod
    implicit none
    type(MyType) :: h, g, v(2), w(2)
    type(MyType), allocatable :: p, q
    type(Child) :: c
    type(Outer) :: o, o2
    integer :: k

    if (allocated(h%arr)) error stop 1
    allocate(Cell :: h%arr(3))
    if (size(h%arr) /= 3 .or. total(h%arr) /= 21) error stop 2
    call fill(h%arr)
    if (h%arr(2)%get() /= 20 .or. holder_total(h) /= 60) error stop 3
    g = h
    call h%arr(1)%set(1)
    if (total(g%arr) /= 60 .or. total(h%arr) /= 51) error stop 4
    call make(h%arr, 4)
    if (total(h%arr) /= 100) error stop 5
    finals = 0
    g = h
    if (finals /= 3) error stop 6
    call h%arr(1)%set(1)
    if (total(g%arr) /= 100 .or. total(h%arr) /= 91) error stop 7
    deallocate(h%arr)
    if (allocated(h%arr)) error stop 8
    allocate(h%arr(2), source=Cell(5))
    if (total(h%arr) /= 10) error stop 9
    h = h
    if (total(h%arr) /= 10) error stop 10
    finals = 0
    call reset(h)
    if (allocated(h%arr) .or. finals /= 2) error stop 11
    h = MyType(3)
    if (h%tag /= 3 .or. allocated(h%arr)) error stop 12

    call make(v(1)%arr, 2)
    if (total(v(1)%arr) /= 30 .or. allocated(v(2)%arr)) error stop 13
    w = v
    if (total(w(1)%arr) /= 30 .or. allocated(w(2)%arr)) error stop 14
    w(1) = w(2)
    if (allocated(w(1)%arr)) error stop 15
    do k = 1, 2
        allocate(w(k)%arr(k), source=Cell(k))
    end do
    if (total(w(2)%arr) /= 4) error stop 16

    allocate(p)
    allocate(p%arr(1), source=Cell(9))
    q = p
    deallocate(p)
    if (total(q%arr) /= 9) error stop 17

    allocate(c%arr(2), source=Cell(11))
    if (total(c%arr) /= 22 .or. c%arr(2)%get() /= 11) error stop 18

    allocate(Cell :: o%inner%arr(2))
    allocate(Cell :: o%grid(2, 3))
    if (size(o%grid) /= 6 .or. o%grid(2, 3)%get() /= 7) error stop 19
    o2 = o
    if (total(o2%inner%arr) /= 14 .or. o2%grid(1, 2)%get() /= 7) error stop 20
    finals = 0
    deallocate(o%grid)
    if (finals /= 6) error stop 21

    allocate(global%arr(3), source=Cell(2))
    if (total(global%arr) /= 6) error stop 22
    call make(global%arr, 1)
    if (total(global%arr) /= 10) error stop 23

    call nested()
    print *, "ok"

contains

    subroutine nested()
        type(MyType) :: local
        allocate(Wide :: local%arr(5))
        call fill(local%arr)
        if (total(local%arr) /= 150) error stop 24
        do k = 1, 5
            if (local%arr(k)%get() /= 10 * k) error stop 25
        end do
    end subroutine nested
end program traits_runtime_array_component_01
