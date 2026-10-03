! Only global_init_19_s uses this module; see global_init_19.f90.
module global_init_19_m
    implicit none
    integer, target :: tgt(4) = [1, 2, 3, 4]
    integer, pointer :: pt(:) => tgt
    integer, pointer :: psec(:) => tgt(2:3)
end module global_init_19_m
