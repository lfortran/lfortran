program infer_walrus_struct_03
    implicit none
    type :: vec_t
        integer :: x, y, z
    end type
    p = vec_t(1, 2, 3)
    if (p%x /= 1) error stop
    if (p%y /= 2) error stop
    if (p%z /= 3) error stop
    print *, "PASSED"
end program
