program infer_walrus_struct_01
    implicit none
    type :: point_t
        real :: x, y
    end type
    p := point_t(1.0, 2.0)
    if (abs(p%x - 1.0) > 1.0e-6) error stop
    if (abs(p%y - 2.0) > 1.0e-6) error stop
    print *, "PASSED"
end program
