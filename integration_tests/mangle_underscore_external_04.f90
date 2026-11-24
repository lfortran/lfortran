! BIND(C) binding labels are not underscore-mangled by
! --mangle-underscore-external (#18545). peak_val is defined in C.
program mangle_underscore_external_04
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none
    interface
        function peak_val() bind(c)
            import :: c_double
            real(c_double) :: peak_val
        end function
    end interface
    real(c_double) :: v
    v = peak_val()
    print *, v
    if (v /= 42.0_c_double) error stop 1
end program
