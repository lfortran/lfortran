module omp_threadprivate_multi_mod
    implicit none

    integer :: iuser_mw = 1
    real :: ruser_mw = 2.0
    !$omp threadprivate(iuser_mw, ruser_mw)

end module omp_threadprivate_multi_mod

program openmp_75
    use omp_threadprivate_multi_mod, only: iuser_mw, ruser_mw
    implicit none

    if (iuser_mw /= 1) error stop
    if (abs(ruser_mw - 2.0) > 1e-6) error stop

    iuser_mw = 42
    ruser_mw = 3.5

    if (iuser_mw /= 42) error stop
    if (abs(ruser_mw - 3.5) > 1e-6) error stop
end program openmp_75
