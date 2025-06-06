module omp_lib
use iso_c_binding, only: c_funptr, c_ptr, c_int
implicit none

interface
subroutine GOMP_parallel (fn, data, num_threads, flags) bind (C, name="GOMP_parallel")
import :: c_funptr, c_ptr, c_int
type(c_funptr), value :: fn
type(c_ptr), value :: data
integer(c_int), value :: num_threads
integer(c_int), value :: flags
end subroutine

integer(c_int) function GOMP_sections_start(count) bind(C, name="GOMP_sections_start")
    import :: c_int
    integer(c_int), value :: count
end function GOMP_sections_start

integer(c_int) function GOMP_sections_next() bind(C, name="GOMP_sections_next")
    import :: c_int
end function GOMP_sections_next

subroutine GOMP_sections_end() bind(C, name="GOMP_sections_end")
end subroutine GOMP_sections_end

subroutine GOMP_barrier() bind(C, name="GOMP_barrier")
end subroutine

subroutine GOMP_critical_start() bind(C, name="GOMP_critical_start")
end subroutine

subroutine GOMP_critical_end() bind(C, name="GOMP_critical_end")
end subroutine

function omp_get_max_threads() bind(c, name="omp_get_max_threads")
import :: c_int
integer(c_int) :: omp_get_max_threads
end function omp_get_max_threads

function omp_get_thread_num() bind(c, name="omp_get_thread_num")
import :: c_int
integer(c_int) :: omp_get_thread_num
end function omp_get_thread_num

subroutine omp_set_num_threads(n) bind(c, name="omp_set_num_threads")
import :: c_int
integer(c_int), value :: n
end subroutine omp_set_num_threads

subroutine GOMP_atomic_start() bind(C, name="GOMP_atomic_start")
end subroutine

subroutine GOMP_atomic_end() bind(C, name="GOMP_atomic_end")
end subroutine

double precision function omp_get_wtime() bind(c, name="omp_get_wtime")
end function omp_get_wtime

integer function omp_get_num_procs() bind(c, name="omp_get_num_procs")
end function omp_get_num_procs

end interface

end module
