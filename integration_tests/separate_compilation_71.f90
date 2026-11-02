! Two units that use iso_fortran_env and open(newunit=) both compile the
! intrinsic module procedures; with --mangle-underscore their names are the
! same in every unit (#16082)
program separate_compilation_71
    use separate_compilation_71a, only: fa
    use separate_compilation_71b, only: fb
    implicit none
    if (fa() /= 1) error stop
    if (fb() /= 2) error stop
    print *, fa() + fb()
end program
