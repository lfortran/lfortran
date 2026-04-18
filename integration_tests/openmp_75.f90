module omp_threadprivate_multi_mod
    use iso_c_binding
    implicit none

    interface
        subroutine GOMP_parallel(fn, data, num_threads, flags) &
                bind(c, name="GOMP_parallel")
            import :: c_funptr, c_int, c_ptr
            type(c_funptr), value :: fn
            type(c_ptr), value :: data
            integer(c_int), value :: num_threads
            integer(c_int), value :: flags
        end subroutine GOMP_parallel

        subroutine GOMP_barrier() bind(c, name="GOMP_barrier")
        end subroutine GOMP_barrier

        function omp_get_thread_num() bind(c, name="omp_get_thread_num")
            import :: c_int
            integer(c_int) :: omp_get_thread_num
        end function omp_get_thread_num
    end interface

    integer :: iuser_mw = -1
    real :: ruser_mw = -1.0
    !$omp threadprivate(iuser_mw, ruser_mw)

contains

    subroutine set_values(tid)
        integer, intent(in) :: tid

        iuser_mw = tid + 100
        ruser_mw = real(tid) + 0.25
    end subroutine set_values

    subroutine check_values(tid)
        integer, intent(in) :: tid

        if (iuser_mw /= tid + 100) error stop
        if (abs(ruser_mw - (real(tid) + 0.25)) > 1e-6) error stop
    end subroutine check_values

end module omp_threadprivate_multi_mod

subroutine lcompilers_threadprivate_worker() bind(c)
    use omp_threadprivate_multi_mod, only: GOMP_barrier, check_values, &
        omp_get_thread_num, set_values
    implicit none

    integer :: tid

    tid = omp_get_thread_num()
    call set_values(tid)
    call GOMP_barrier()
    call check_values(tid)
end subroutine lcompilers_threadprivate_worker

program openmp_75
    use iso_c_binding
    use omp_threadprivate_multi_mod, only: GOMP_parallel
    implicit none

    interface
        subroutine lcompilers_threadprivate_worker() bind(c)
        end subroutine lcompilers_threadprivate_worker
    end interface

    call GOMP_parallel(c_funloc(lcompilers_threadprivate_worker), &
        c_null_ptr, 4_c_int, 0_c_int)
end program openmp_75
