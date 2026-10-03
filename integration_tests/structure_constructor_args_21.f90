! A structure constructor for an extended type whose first positional
! argument is a value of an ancestor type that the nonpolymorphic first
! component cannot hold (it is not of the component's declared type) gives
! the parent component (an extension, see structure_constructor_args_14).
module structure_constructor_args_21_mod
    implicit none

    type :: root_t
        type(root_t), pointer :: next => null()
        integer :: v = 1
    end type

    type, extends(root_t) :: mid_t
        integer :: m = 2
    end type

    type, extends(mid_t) :: leaf_t
        integer :: d1 = 3
    end type
end module

program structure_constructor_args_21
    use structure_constructor_args_21_mod
    implicit none
    type(root_t), target :: r
    type(mid_t) :: mid
    type(leaf_t) :: leaf

    r%v = 10
    mid%next => r
    mid%v = 11
    mid%m = 12
    leaf = leaf_t(mid, 5)
    if (.not. associated(leaf%next, r)) error stop 1
    if (leaf%next%v /= 10) error stop 2
    if (leaf%v /= 11) error stop 3
    if (leaf%m /= 12) error stop 4
    if (leaf%d1 /= 5) error stop 5
    print *, "ok"
end program
