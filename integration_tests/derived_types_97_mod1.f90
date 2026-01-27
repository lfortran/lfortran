module derived_types_97_mod1
    implicit none
    
    type :: point_t
        integer :: x
        integer :: y
    end type point_t

    interface point_t
        module procedure point_new
    end interface point_t

contains

    type(point_t) function point_new() result(p)
        p%x = 0
        p%y = 0
    end function point_new
    
end module derived_types_97_mod1
