! A structure constructor for an extended type whose first positional
! argument is a value of an ancestor type gives that value to the first
! component when the component can hold it, as the standard says, and not
! the parent component (an extension, see structure_constructor_args_14).
! GFortran always takes the parent component here, so this test is not
! labeled gfortran.
module structure_constructor_args_20_mod
    implicit none

    type :: any_base_t
        class(*), allocatable :: x
        integer :: v = 1
    end type

    type, extends(any_base_t) :: any_der_t
        integer :: d1 = 3
    end type

    type :: tree_base_t
        class(tree_base_t), allocatable :: child
        integer :: v = 1
    end type

    type, extends(tree_base_t) :: tree_der_t
        integer :: d1 = 3
    end type

    type :: root_t
        class(root_t), allocatable :: next
        integer :: v = 1
    end type

    type, extends(root_t) :: mid_t
        integer :: m = 2
    end type

    type, extends(mid_t) :: leaf_t
        integer :: d1 = 3
    end type
end module

program structure_constructor_args_20
    use structure_constructor_args_20_mod
    implicit none
    type(any_base_t) :: ab
    type(any_der_t) :: ad
    type(tree_base_t) :: tb
    type(tree_der_t) :: td
    type(mid_t) :: mid
    type(leaf_t) :: leaf

    ! A class(*) component holds a value of any type.
    allocate(ab%x, source=5)
    ab%v = 99
    ad = any_der_t(ab, 7)
    if (.not. allocated(ad%x)) error stop 1
    select type (p => ad%x)
    type is (any_base_t)
        if (p%v /= 99) error stop 2
        if (.not. allocated(p%x)) error stop 3
    class default
        error stop 4
    end select
    if (ad%v /= 7) error stop 5
    if (ad%d1 /= 3) error stop 6

    ad = any_der_t(ab, 8, 9)
    select type (p => ad%x)
    type is (any_base_t)
        if (p%v /= 99) error stop 7
    class default
        error stop 8
    end select
    if (ad%v /= 8) error stop 9
    if (ad%d1 /= 9) error stop 10

    ! A class(tree_base_t) component holds a value of type tree_base_t.
    tb%v = 42
    td = tree_der_t(tb, 7)
    if (.not. allocated(td%child)) error stop 11
    select type (c => td%child)
    type is (tree_base_t)
        if (c%v /= 42) error stop 12
    class default
        error stop 13
    end select
    if (td%v /= 7) error stop 14
    if (td%d1 /= 3) error stop 15

    ! A class(root_t) component holds a value of type mid_t, which extends
    ! root_t.
    mid%v = 11
    mid%m = 12
    leaf = leaf_t(mid, 5)
    if (.not. allocated(leaf%next)) error stop 16
    select type (n => leaf%next)
    type is (mid_t)
        if (n%v /= 11) error stop 17
        if (n%m /= 12) error stop 18
    class default
        error stop 19
    end select
    if (leaf%v /= 5) error stop 20
    if (leaf%m /= 2) error stop 21
    if (leaf%d1 /= 3) error stop 22

    print *, "ok"
end program
