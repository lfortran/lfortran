program openmp_bindc_threadprivate
    use openmp_bindc_threadprivate_mod
    implicit none
    if (read_counter() /= 7) error stop
    bindc_tls_counter = 19
    if (bindc_tls_counter /= 19) error stop
    if (read_counter() /= 19) error stop
end program
