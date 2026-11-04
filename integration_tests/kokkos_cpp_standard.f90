program kokkos_cpp_standard
    integer :: answer
    answer = 42
    if (answer /= 42) error stop
    print *, answer
end program
