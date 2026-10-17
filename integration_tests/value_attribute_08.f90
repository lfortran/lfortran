! A VALUE dummy is the callee's own copy of the actual argument, so the caller
! makes no copy of a TYPE IS selector or of a component bound to it: no
! temporary is finalized, and the callee's changes do not reach the actual.
! Not labeled gfortran: GFortran copies VALUE dummies shallowly (#14267).
module value_attribute_08_m
    implicit none
    integer :: nfin = 0
    type, abstract :: base_t
    end type
    type, extends(base_t) :: item_t
        integer :: v = 0
        integer, allocatable :: a(:)
    contains
        final :: fin_item
    end type
    type :: holder_t
        type(item_t), allocatable :: it
    end type
contains
    subroutine fin_item(x)
        type(item_t), intent(inout) :: x
        nfin = nfin + 1
    end subroutine

    subroutine take(x)
        type(item_t), value :: x
        if (x%v /= 8 .or. any(x%a /= [1, 2])) error stop 1
        x%v = 100
        x%a(1) = 99
        deallocate(x%a)
    end subroutine
end module

program value_attribute_08
    use value_attribute_08_m
    implicit none
    type(item_t) :: y
    type(holder_t) :: h
    class(base_t), allocatable :: s

    y%v = 8
    allocate(y%a(2))
    y%a = [1, 2]
    nfin = 0
    call take(y)
    if (nfin /= 0) error stop 5
    if (y%v /= 8 .or. any(y%a /= [1, 2])) error stop 2

    allocate(h%it)
    h%it%v = 8
    allocate(h%it%a(2))
    h%it%a = [1, 2]
    nfin = 0
    call take(h%it)
    if (nfin /= 0) error stop 6
    if (h%it%v /= 8 .or. any(h%it%a /= [1, 2])) error stop 3

    allocate(item_t :: s)
    select type (s)
    type is (item_t)
        s%v = 8
        allocate(s%a(2))
        s%a = [1, 2]
        nfin = 0
        call take(s)
        if (nfin /= 0) error stop 7
        if (s%v /= 8 .or. any(s%a /= [1, 2])) error stop 4
    end select
end program
