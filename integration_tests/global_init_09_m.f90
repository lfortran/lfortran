module global_init_09_m
    implicit none
    type :: t
        integer, pointer :: p(:) => null()
    end type
    type(t) :: mscal
end module global_init_09_m
