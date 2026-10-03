! Initializes entities from the imported parameters of another module,
! whose procedure-pointer interfaces are not imported here, or are imported
! under another name.
module structure_constructor_args_08_b
    use structure_constructor_args_08_a, only: o_t, z, z2, ff => fi
    implicit none
    type(o_t) :: mv = z
    type(o_t) :: mv2 = z2
    procedure(ff), pointer :: extra => null()
    type :: holder_t
        type(o_t) :: h = z2
    end type
contains
    integer function local_x()
        type(o_t) :: y = z
        local_x = y%x
        if (associated(y%fp) .or. associated(y%gp)) local_x = -1
        y%gp => twice
        if (y%gp(4) /= 8) local_x = -2
    end function

    integer function twice(i)
        integer, intent(in) :: i
        twice = 2*i
    end function
end module
