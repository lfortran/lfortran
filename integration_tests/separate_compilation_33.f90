program separate_compilation_33
    use separate_compilation_33a, only: divergence_1d_t
    implicit none

    type(divergence_1d_t) :: x

    if (storage_size(x) <= 0) error stop 1
end program separate_compilation_33
