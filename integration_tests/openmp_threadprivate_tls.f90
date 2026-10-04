subroutine openmp_threadprivate_tls_worker(data) bind(c)
    use openmp_threadprivate_tls_mod
    implicit none
    type(c_ptr), value :: data
    integer :: tid, turn

    if (omp_get_num_threads() /= 4) error stop
    tid = omp_get_thread_num()
    if (tid < 0 .or. tid > 3) error stop
    if (iuser_mw /= -1) error stop
    if (ruser_mw /= -1.0) error stop
    call GOMP_barrier()

    ! Order the writes so missing TLS fails without a shared-write race.
    do turn = 0, 3
        if (tid == turn) call set_values(tid)
        call GOMP_barrier()
    end do
    call check_values(tid)
    observed(tid) = iuser_mw
end subroutine openmp_threadprivate_tls_worker

program openmp_threadprivate_tls
    use openmp_threadprivate_tls_mod
    implicit none
    integer :: tid

    interface
        subroutine openmp_threadprivate_tls_worker(data) bind(c)
            import :: c_ptr
            type(c_ptr), value :: data
        end subroutine openmp_threadprivate_tls_worker
    end interface

    call omp_set_dynamic(0_c_int)
    call GOMP_parallel(c_funloc(openmp_threadprivate_tls_worker), &
        c_null_ptr, 4_c_int, 0_c_int)
    do tid = 0, 3
        if (observed(tid) /= tid + 100) error stop
    end do
    call check_values(0)
end program openmp_threadprivate_tls
