module derived_types_59_m
    implicit none
    type inside
        integer :: m = 0
    end type inside

    type wrapper
        type(inside) :: i = inside()
    end type
end module

program derived_types_59
    use derived_types_59_m
    implicit none

    type(wrapper) :: w
    w = wrapper()

    if (w%i%m /= 0) error stop
end program
