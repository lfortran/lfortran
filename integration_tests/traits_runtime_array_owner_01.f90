! Allocatable runtime trait arrays (TD-46) own elements of one dynamic type,
! as the reported `class(ITraitOne), allocatable, intent(out) :: arr(:)`
! message does: an implementation allocates the elements with a type
! specification or a SOURCE, also with lower bounds, and the caller's local
! array, passed statically and through a runtime view, is inquired with
! `allocated`, `size`, `lbound` and `ubound`, its elements dispatch, it is
! passed to a borrowed trait array dummy and to an allocatable one that
! reallocates it, and it is deallocated explicitly,
! on reallocation through intent(out) and at the end of its scope, finalizing
! the allocatable components of its elements.
module traits_runtime_array_owner_01_traits
    implicit none
    private
    public :: ITraitOne, ITraitTwo

    abstract interface :: ITraitOne
        integer function get(self)
            type(itself), pass, intent(in) :: self
        end function get
        subroutine set(self, v)
            type(itself), pass, intent(inout) :: self
            integer, intent(in) :: v
        end subroutine set
    end interface

    abstract interface :: ITraitTwo
        subroutine method(self, arr)
            type(itself), pass, intent(in) :: self
            class(ITraitOne), allocatable, intent(out) :: arr(:)
        end subroutine method
    end interface
end module traits_runtime_array_owner_01_traits

module traits_runtime_array_owner_01_impl
    use traits_runtime_array_owner_01_traits, only: ITraitOne, ITraitTwo
    implicit none
    private
    public :: Cell, Wide, Maker, total, fill, regrow

    type, implements(ITraitOne) :: Cell
        integer :: v = 7
    contains
        procedure :: get => cell_get
        procedure :: set => cell_set
    end type Cell

    ! A larger element with an allocatable component.
    type, implements(ITraitOne) :: Wide
        real(8) :: pad(3) = 0
        integer, allocatable :: v(:)
    contains
        procedure :: get => wide_get
        procedure :: set => wide_set
    end type Wide

    type, implements(ITraitTwo) :: Maker
        integer :: n = 3
        logical :: wide = .false.
    contains
        procedure :: method => maker_method
    end type Maker

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
        self%pad = 0
    end subroutine wide_set

    subroutine maker_method(self, arr)
        class(Maker), intent(in) :: self
        class(ITraitOne), allocatable, intent(out) :: arr(:)
        if (allocated(arr)) error stop 101
        if (self%wide) then
            allocate(Wide :: arr(0:self%n - 1))
        else
            allocate(Cell :: arr(self%n))
        end if
        call fill(arr)
    end subroutine maker_method

    subroutine fill(arr)
        class(ITraitOne), intent(inout) :: arr(:)
        integer :: i
        do i = 1, size(arr)
            call arr(i)%set(10 * i)
        end do
    end subroutine fill

    ! Reallocates an owner it is passed, keeping its dynamic type.
    subroutine regrow(arr, n)
        class(ITraitOne), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: n
        if (allocated(arr)) deallocate(arr)
        allocate(Wide :: arr(n))
        call fill(arr)
    end subroutine regrow

    integer function total(arr) result(s)
        class(ITraitOne), intent(in) :: arr(:)
        integer :: i
        s = 0
        do i = 1, size(arr)
            s = s + arr(i)%get()
        end do
    end function total
end module traits_runtime_array_owner_01_impl

program traits_runtime_array_owner_01
    use traits_runtime_array_owner_01_traits, only: ITraitOne, ITraitTwo
    use traits_runtime_array_owner_01_impl
    implicit none
    class(ITraitOne), allocatable :: arr(:)
    class(ITraitTwo), allocatable :: two
    type(Maker) :: mk
    integer :: i

    if (allocated(arr)) error stop 1
    call mk%method(arr)
    if (.not. allocated(arr)) error stop 2
    if (size(arr) /= 3 .or. lbound(arr, 1) /= 1 .or. ubound(arr, 1) /= 3) error stop 3
    if (total(arr) /= 60) error stop 4
    if (arr(2)%get() /= 20) error stop 5

    two = Maker(4, .true.)
    call two%method(arr)
    if (size(arr) /= 4 .or. lbound(arr, 1) /= 0 .or. ubound(arr, 1) /= 3) error stop 6
    if (arr(0)%get() /= 10 .or. arr(3)%get() /= 40) error stop 7
    if (total(arr) /= 100) error stop 8
    call arr(1)%set(5)
    if (arr(1)%get() /= 5 .or. total(arr) /= 85) error stop 9

    deallocate(arr)
    if (allocated(arr)) error stop 10
    allocate(arr(2), source=Cell(5))
    if (arr(1)%get() /= 5 .or. arr(2)%get() /= 5) error stop 11
    deallocate(arr)
    allocate(Cell :: arr(2))
    if (total(arr) /= 14) error stop 12
    call regrow(arr, 3)
    if (size(arr) /= 3 .or. total(arr) /= 60) error stop 15
    call nested()
    print *, "ok"

contains

    subroutine nested()
        class(ITraitOne), allocatable :: local(:)
        allocate(Wide :: local(5))
        call fill(local)
        if (total(local) /= 150) error stop 13
        do i = 1, 5
            if (local(i)%get() /= 10 * i) error stop 14
        end do
    end subroutine nested
end program traits_runtime_array_owner_01
