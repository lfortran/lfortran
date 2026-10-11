module openmp_threadprivate_tls_mod
    use iso_c_binding
    implicit none

    interface
        subroutine GOMP_parallel(fn, data, num_threads, flags) &
                bind(c, name="GOMP_parallel")
            import :: c_funptr, c_int, c_ptr
            type(c_funptr), value :: fn
            type(c_ptr), value :: data
            integer(c_int), value :: num_threads, flags
        end subroutine GOMP_parallel

        subroutine GOMP_barrier() bind(c, name="GOMP_barrier")
        end subroutine GOMP_barrier

        subroutine omp_set_dynamic(dynamic) bind(c, name="omp_set_dynamic")
            import :: c_int
            integer(c_int), value :: dynamic
        end subroutine omp_set_dynamic

        function omp_get_thread_num() bind(c, name="omp_get_thread_num")
            import :: c_int
            integer(c_int) :: omp_get_thread_num
        end function omp_get_thread_num

        function omp_get_num_threads() bind(c, name="omp_get_num_threads")
            import :: c_int
            integer(c_int) :: omp_get_num_threads
        end function omp_get_num_threads
    end interface

    integer, save :: iuser_mw = -1
    real :: ruser_mw = -1.0
    !$OmP ThReAdPrIvAtE	(iuser_mw, ruser_mw)
    integer :: observed(0:3) = -1

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

end module openmp_threadprivate_tls_mod
