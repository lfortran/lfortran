module derived_types_258_m1
    implicit none
    type :: flag
        integer :: value
    end type
    type(flag), parameter :: overflow = flag(2)
    type(flag), parameter :: underflow = flag(4)
end module
