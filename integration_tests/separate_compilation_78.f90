! A module procedure and a module variable defined in a unit compiled
! separately with --module-mangling link with the unit that uses them
! (#17306, #16381)
program separate_compilation_78
    use separate_compilation_78a, only: triple, calls
    implicit none
    if (triple(14) /= 42) error stop
    if (calls /= 1) error stop
    print *, triple(1), calls
end program
