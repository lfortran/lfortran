module openmp_bindc_threadprivate_mod
    use iso_c_binding, only: c_int
    implicit none
    integer(c_int), bind(c) :: bindc_tls_counter = 7
    !$omp threadprivate(bindc_tls_counter)
contains
    integer function read_counter()
        read_counter = bindc_tls_counter
    end function
end module
