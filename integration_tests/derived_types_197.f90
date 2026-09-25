program derived_types_197
    implicit none

    type :: base_t
        integer :: before = 1
        integer :: after = 2
    end type base_t

    type, extends(base_t) :: child_t
        integer :: extra = 3
    end type child_t

    type, extends(child_t) :: grandchild_t
        integer :: more = 4
    end type grandchild_t

    type :: holder_t
        type(child_t) :: c
        integer :: k = 5
    end type holder_t

    integer :: i
    integer :: int_bits
    type(base_t) :: b
    type(child_t) :: c
    type(grandchild_t) :: g
    type(holder_t) :: h
    type(child_t) :: carr(3)
    type(child_t), allocatable :: calloc

    i = 0
    int_bits = storage_size(i)

    ! An extended type can never be smaller than the type it extends.
    if (storage_size(c) < storage_size(b)) error stop "child smaller than parent"
    if (storage_size(g) < storage_size(c)) error stop "grandchild smaller than child"
    if (storage_size(h) < storage_size(c)) error stop "holder smaller than its component"

    ! Every component here is a default integer, so the components pack with
    ! no padding and each level of extension adds exactly one integer.
    if (storage_size(b) /= 2 * int_bits) error stop "wrong size for base type"
    if (storage_size(c) /= storage_size(b) + int_bits) error stop "parent components missing from extended type"
    if (storage_size(g) /= storage_size(c) + int_bits) error stop "parent components missing from twice extended type"

    ! A component of an extended type contributes its whole size, parent
    ! components included.
    if (storage_size(h) /= storage_size(c) + int_bits) error stop "parent components missing from extended type component"

    ! storage_size reports the size of one element for arrays and the size of
    ! the declared type for allocatables.
    if (storage_size(carr) /= storage_size(c)) error stop "wrong element size for array of extended type"
    if (storage_size(carr(2)) /= storage_size(c)) error stop "wrong size for element of array of extended type"

    allocate(calloc)
    if (storage_size(calloc) /= storage_size(c)) error stop "wrong size for allocatable of extended type"
    deallocate(calloc)

    print *, storage_size(b), storage_size(c), storage_size(g), storage_size(h)
end program derived_types_197
