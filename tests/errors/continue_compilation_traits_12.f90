! Runtime trait arrays (TD-46) are assumed-shape dummies that borrow a
! nonpolymorphic derived-type array in place, and deferred-shape allocatable
! dummies, local variables and components that own their elements.
module continue_compilation_traits_12_m
    implicit none
    private
    public :: ISomeTrait, IOther, Item, Other, use_items, use_other, touch_items, look_items

    abstract interface :: ISomeTrait
        integer function value(self)
            type(itself), pass, intent(in) :: self
        end function value
    end interface

    abstract interface :: IOther
        integer function other(self)
            type(itself), pass, intent(in) :: self
        end function other
    end interface

    type, implements(ISomeTrait) :: Item
        integer :: v = 0
    contains
        procedure :: value => item_value
    end type Item

    type, implements(IOther) :: Other
    contains
        procedure :: other => other_other
    end type Other

contains

    integer function item_value(self)
        class(Item), intent(in) :: self
        item_value = self%v
    end function item_value

    integer function other_other(self)
        class(Other), intent(in) :: self
        other_other = 0
    end function other_other

    integer function use_items(items) result(s)
        class(ISomeTrait), intent(in) :: items(:)
        s = size(items)
    end function use_items

    subroutine touch_items(items)
        class(ISomeTrait), intent(inout) :: items(:)
    end subroutine touch_items

    subroutine look_items(items)
        class(ISomeTrait), intent(in) :: items(:)
    end subroutine look_items

    integer function use_other(items) result(s)
        class(IOther), intent(in) :: items(:)
        s = size(items)
    end function use_other
end module continue_compilation_traits_12_m

module continue_compilation_traits_12_storage
    use continue_compilation_traits_12_m, only: ISomeTrait, Item, use_items
    implicit none
    ! Allocatable trait arrays are dummies, local variables and components.
    class(ISomeTrait), allocatable :: saved(:)
    type :: Holder
        class(ISomeTrait), allocatable :: items(:)
        ! A component trait array is allocatable.
        class(ISomeTrait), pointer :: linked(:)
    end type Holder
    type :: Box
        class(ISomeTrait), allocatable :: items(:)
    end type Box
contains
    subroutine owners(a, b)
        ! Only assumed-shape dummies without allocatable or pointer, and
        ! deferred-shape allocatable ones.
        class(ISomeTrait), pointer, intent(inout) :: a(:)
        class(ISomeTrait), intent(in) :: b(3)
    end subroutine owners

    subroutine allocations(c, d, e)
        class(ISomeTrait), allocatable, intent(inout) :: c(:), d(:)
        class(ISomeTrait), allocatable, intent(in) :: e(:)
        ! The bounds of each dimension are required.
        allocate(c, source=Item(1))
        ! A mold is not implemented.
        allocate(d(2), mold=Item(1))
        ! An intent(in) owner cannot be allocated.
        allocate(Item :: e(2))
    end subroutine allocations

    subroutine components(f)
        type(Box), intent(in) :: f
        type(Box) :: g
        ! A component of an intent(in) structure cannot be allocated or
        ! deallocated.
        allocate(Item :: f%items(2))
        deallocate(f%items)
        ! A constructor value of a trait array component is not implemented.
        g = Box(f%items)
    end subroutine components

end module continue_compilation_traits_12_storage

module continue_compilation_traits_12_inspection
    use continue_compilation_traits_12_m, only: ISomeTrait, Item
    implicit none
contains
    ! Concrete inspection of an element is not implemented.
    integer function pick(items) result(r)
        class(ISomeTrait), intent(in) :: items(:)
        r = 0
        select type (e => items(1))
        type is (Item)
            r = e%v
        end select
    end function pick
end module continue_compilation_traits_12_inspection

program continue_compilation_traits_12
    use continue_compilation_traits_12_m
    implicit none
    type(Item) :: items(4), matrix(2, 2)
    type(Other) :: others(2)
    class(Item), allocatable :: poly(:)
    class(ISomeTrait), allocatable :: owned(:)
    integer :: n

    n = use_items(items)
    ! A vector subscript has no in-place storage to borrow.
    n = use_items(items([1, 3]))
    ! The rank of the actual must be the dummy's.
    n = use_items(matrix)
    ! A polymorphic array may hold extensions of another size.
    allocate(poly(2))
    n = use_items(poly)
    ! Its element type must implement the dummy's trait.
    n = use_items(others)
    n = use_other(others)
    ! A section of a trait array is passed in place, but not one with a
    ! vector subscript yet.
    allocate(Item :: owned(2))
    n = use_items(owned)
    call touch_items(owned(1:1))
    call touch_items(owned([2, 1]))
    call look_items(owned([2, 1]))
end program continue_compilation_traits_12
