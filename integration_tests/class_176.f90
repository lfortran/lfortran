! An extension's intrinsic array assignment must copy components.
! The ancestor's deferred assignment(=) is an abstract interface and
! has no procedure to call.
module class_176_mod
    implicit none

    type, abstract :: item
    contains
        generic :: assignment(=) => assign_equal
        procedure(assign_equal_iface), deferred :: assign_equal
    end type item

    abstract interface
        subroutine assign_equal_iface(v1, v2)
            import :: item
            class(item), intent(out) :: v1
            class(item), intent(in) :: v2
        end subroutine assign_equal_iface
    end interface

    type, extends(item) :: myint
        integer :: i = 0
    contains
        procedure :: assign_equal => int_assign_equal
    end type myint

contains

    subroutine int_assign_equal(v1, v2)
        class(myint), intent(out) :: v1
        class(item), intent(in) :: v2
        select type (v2)
        class is (myint)
            v1%i = v2%i + 1
        class default
            error stop "assignment not defined"
        end select
    end subroutine int_assign_equal

end module class_176_mod

program class_176
    use class_176_mod
    implicit none

    type(myint) :: a, b
    type(myint) :: list(2), copied(2)
    class(item), allocatable :: elems(:)

    a%i = 4
    b = a
    if (b%i /= 5) error stop "scalar defined assignment was not used"

    list%i = [4, 7]
    copied = list
    if (copied(1)%i /= 4 .or. copied(2)%i /= 7) error stop "array copy"

    allocate(elems(2), source=list)
    select type (elems)
    type is (myint)
        copied = elems
    end select
    if (copied(1)%i /= 4 .or. copied(2)%i /= 7) error stop "select type array copy"
end program class_176
