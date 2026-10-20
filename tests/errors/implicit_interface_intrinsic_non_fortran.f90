module implicit_interface_intrinsic_non_fortran_mod
    intrinsic :: fma
end module implicit_interface_intrinsic_non_fortran_mod

program implicit_interface_intrinsic_non_fortran
    use implicit_interface_intrinsic_non_fortran_mod
    intrinsic trunc, sin
    real, intrinsic :: exp2
    double precision, intrinsic :: cos
    print *, sin(1.0), cos(1.0d0)
end program implicit_interface_intrinsic_non_fortran
