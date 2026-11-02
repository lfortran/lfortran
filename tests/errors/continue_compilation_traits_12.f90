! Runtime trait arrays (TD-46) are assumed-shape dummies that borrow a
! nonpolymorphic derived-type array in place.
module continue_compilation_traits_12_m
    implicit none
    private
    public :: ISomeTrait, IOther, Item, Other, use_items, use_other

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

    integer function use_other(items) result(s)
        class(IOther), intent(in) :: items(:)
        s = size(items)
    end function use_other
end module continue_compilation_traits_12_m

module continue_compilation_traits_12_storage
    use continue_compilation_traits_12_m, only: ISomeTrait
    implicit none
contains
    subroutine owners(a, b)
        ! Only assumed-shape dummies without allocatable or pointer.
        class(ISomeTrait), allocatable, intent(inout) :: a(:)
        class(ISomeTrait), intent(in) :: b(3)
    end subroutine owners
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
end program continue_compilation_traits_12
