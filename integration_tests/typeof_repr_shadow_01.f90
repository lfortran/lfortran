program typeof_repr_shadow_01
    implicit none

    type :: type_info
        integer :: i
    end type type_info

    type(type_info) :: x
    x%i = 1
    if (x%i /= 1) error stop 1
    print *, x%i
end program typeof_repr_shadow_01
