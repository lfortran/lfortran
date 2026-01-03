module derived_types_45_mod
    implicit none
    type myint
        integer :: m
    end type
 end module derived_types_45_mod

program derived_types_45
    use derived_types_45_mod

    implicit none
    type(myint), allocatable :: ins
    ins = myint(44)

    if (ins%m /= 44) error stop
end program derived_types_45
