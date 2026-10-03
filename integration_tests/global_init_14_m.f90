! Only global_init_14_s uses this module; see global_init_14.f90.
module global_init_14_m
    implicit none
    type :: t
        integer, pointer :: p(:) => null()
    end type
    type(t) :: mscal
    integer, allocatable :: a(:)
end module global_init_14_m
