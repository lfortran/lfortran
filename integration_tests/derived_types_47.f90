module derived_types_47_m
    implicit none
    type t
        integer :: x = 1
        integer :: y = 1
    end type

    type(t), parameter :: ins = t(2, 3)

end module derived_types_47_m

program derived_types_47
    use derived_types_47_m
    implicit none

    if (ins%x /= 2) error stop
    if (ins%y /= 3) error stop
end program derived_types_47
