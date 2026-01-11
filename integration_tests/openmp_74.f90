module omp_threadprivate_mod
    implicit none

    integer :: j_start = 1
    !$omp threadprivate(j_start)

contains

    integer function get_j_start()
        implicit none

        get_j_start = j_start
    end function get_j_start
end module omp_threadprivate_mod

program openmp_74
    use omp_threadprivate_mod, only: get_j_start
    implicit none

    integer :: j

    j = get_j_start()
    if (j /= 1) error stop
end program openmp_74
