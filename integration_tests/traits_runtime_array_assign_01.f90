! Intrinsic assignment of a scalar to an allocated runtime trait array
! (F2018 10.2.1.3), as the reported `arr = MyType()` after
! `allocate(MyType :: arr(1))` in an `intent(out)` dummy does: each element
! takes an independent copy of a structure constructor, a variable, a
! borrowed view or an element of the same dynamic type, also with an
! allocatable component, in a local, component and rank-two array, and the
! array keeps its bounds.
module traits_runtime_array_assign_01_m
    implicit none
    private
    public :: ISomeTrait, MyType, Wide, Client, Holder, total, fill

    abstract interface :: ISomeTrait
        integer function get(self)
            type(itself), pass, intent(in) :: self
        end function get
    end interface

    type, implements(ISomeTrait) :: MyType
        integer :: v = 7
    contains
        procedure :: get => my_get
    end type MyType

    type, implements(ISomeTrait) :: Wide
        integer, allocatable :: v(:)
    contains
        procedure :: get => wide_get
    end type Wide

    type :: Client
    contains
        procedure :: method
    end type Client

    type :: Holder
        class(ISomeTrait), allocatable :: items(:)
    end type Holder

contains

    integer function my_get(self)
        class(MyType), intent(in) :: self
        my_get = self%v
    end function my_get

    integer function wide_get(self)
        class(Wide), intent(in) :: self
        wide_get = -1
        if (allocated(self%v)) wide_get = sum(self%v)
    end function wide_get

    subroutine method(self, arr)
        class(Client), intent(in) :: self
        class(ISomeTrait), allocatable, intent(out) :: arr(:)
        allocate(MyType :: arr(1))
        arr = MyType()
    end subroutine method

    integer function total(objs)
        class(ISomeTrait), intent(in) :: objs(:)
        integer :: i
        total = 0
        do i = 1, size(objs)
            total = total + objs(i)%get()
        end do
    end function total

    subroutine fill(arr, obj)
        class(ISomeTrait), allocatable, intent(inout) :: arr(:)
        class(ISomeTrait), intent(in) :: obj
        arr = obj
    end subroutine fill
end module traits_runtime_array_assign_01_m

program traits_runtime_array_assign_01
    use traits_runtime_array_assign_01_m
    implicit none
    type(Client) :: c
    type(Holder) :: h
    class(ISomeTrait), allocatable :: arr(:), grid(:,:)
    type(MyType) :: m
    type(Wide) :: w

    call c%method(arr)
    if (size(arr) /= 1) error stop 1
    if (arr(1)%get() /= 7) error stop 2

    deallocate(arr)
    allocate(MyType :: arr(2:4))
    m%v = 5
    arr = m
    if (lbound(arr, 1) /= 2 .or. ubound(arr, 1) /= 4) error stop 3
    if (total(arr) /= 15) error stop 4
    arr = MyType(v=2)
    if (total(arr) /= 6) error stop 5
    call fill(arr, MyType(v=3))
    if (total(arr) /= 9) error stop 6
    arr = arr(3)
    if (total(arr) /= 9) error stop 7

    allocate(Wide :: grid(2, 3))
    allocate(w%v(3))
    w%v = [1, 2, 3]
    grid = w
    w%v(1) = 100
    if (total(grid(:, 1)) /= 12) error stop 8
    if (grid(2, 3)%get() /= 6) error stop 9
    grid = Wide([4])
    if (grid(1, 2)%get() /= 4) error stop 10

    allocate(Wide :: h%items(3))
    h%items = w
    if (total(h%items) /= 3 * 105) error stop 11
    print *, total(arr), total(h%items)
end program traits_runtime_array_assign_01
