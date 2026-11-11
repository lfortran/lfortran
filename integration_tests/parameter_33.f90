program parameter_33
    ! A parameter may be initialized from an element of a real128 parameter
    ! array. The element value is a compile-time constant even when the array
    ! constant is not packed into an ArrayConstant.
    use iso_fortran_env, only: real128
    implicit none
    integer, parameter :: wp = real128
    real(wp), parameter :: d1mach(2) = [tiny(1.0_wp), huge(1.0_wp)]
    real(wp), parameter :: uflow = d1mach(1)
    real(wp), parameter :: oflow = d1mach(2)
    if (uflow /= tiny(1.0_wp)) error stop 1
    if (oflow /= huge(1.0_wp)) error stop 2
end program
