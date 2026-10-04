! Module state that the specification expressions of global_init_24_probe
! read; see global_init_24.f90. The elements' defaults and the string's
! value are the kind of initial state that may be set up by startup code
! rather than laid out as static data.
module global_init_24_m
    implicit none
    type :: cfg
        integer :: n = 4
    end type
    type :: named
        character(len=5) :: s = "abc"
    end type
    type(cfg) :: cfgs(2)
    type(named) :: nm
end module global_init_24_m
