! `c_null_ptr` is a named constant, not `null()`, so it is a valid structure
! constructor argument for a `type(c_ptr)` component of a parameterized
! derived type, given by keyword or taken from the component default.
module structure_constructor_args_11_m
    use iso_c_binding, only: c_ptr, c_null_ptr
    implicit none
    type :: pt(k)
        integer, kind :: k
        integer(k) :: h
        type(c_ptr) :: p = c_null_ptr
    end type
end module

program structure_constructor_args_11
    use iso_c_binding, only: c_associated, c_null_ptr
    use structure_constructor_args_11_m
    implicit none
    type(pt(4)) :: a

    a = pt(4)(h=11, p=c_null_ptr)
    if (a%h /= 11 .or. c_associated(a%p)) error stop
    a = pt(4)(12)
    if (a%h /= 12 .or. c_associated(a%p)) error stop
    a = pt(4)(13, c_null_ptr)
    if (a%h /= 13 .or. c_associated(a%p)) error stop
    print *, "ok"
end program
