! Two units that use ieee_arithmetic both compile the module's procedures;
! with --mangle-underscore their names are the same in every unit (#16118)
program separate_compilation_75
    use separate_compilation_75a, only: neg_a
    use separate_compilation_75b, only: neg_b
    implicit none
    if (neg_a(3.0) /= -3.0) error stop
    if (neg_b(4.0) /= -4.0) error stop
    print *, neg_a(3.0), neg_b(4.0)
end program
