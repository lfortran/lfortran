module derived_types_48_m
    implicit none
    type t
        integer :: x = 1
        integer :: y = 2
    end type

    type, extends(t) :: ext_t
        type(t) :: ins = t()
    end type

end module derived_types_48_m

program derived_types_48
    use derived_types_48_m
    implicit none

    type(ext_t) :: i
    if (i%ins%x /= 1) error stop
    if (i%ins%y /= 2) error stop
end program derived_types_48
