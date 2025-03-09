program derived_types_43
    implicit none
    type :: rp1d_derived_types_43
        real, dimension(:), pointer :: f
    end type

    type :: sds_derived_types_43
        integer, dimension(3) :: dims
        type(rp1d_derived_types_43), dimension(3) :: scales
        real, dimension(:), pointer :: f
    end type

    type(sds_derived_types_43) :: s
    allocate(s%scales(1)%f(2))
    s%scales(1)%f(2) = 1.0
    s%scales(1)%f(1) = 2.0
    print *, s%scales(1)%f(1), s%scales(1)%f(2)
    if (abs(sum(s%scales(1)%f) - 3.0) > 1e-6) error stop
end program
