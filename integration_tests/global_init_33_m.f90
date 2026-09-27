! Module state that startup code sets up, which global_init_33_e.f90's
! external bind(c) procedure sizes an automatic array from; see
! global_init_33.f90.
module global_init_33_m
implicit none
type :: cfg
    integer :: n = 4
end type
type(cfg) :: cfgs(2)
end module global_init_33_m
