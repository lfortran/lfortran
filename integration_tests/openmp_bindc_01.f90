module module_openmp_bindc_01
use iso_c_binding
implicit none

interface
subroutine GOMP_parallel (fn, data, num_threads, flags) bind (C, name="GOMP_parallel")
import :: c_funptr, c_ptr, c_int
type(c_funptr), value :: fn
type(c_ptr), value :: data
integer(c_int), value :: num_threads
integer(c_int), value :: flags
end subroutine

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

end interface

end module

subroutine lcompilers_function() bind(c)
use module_openmp_bindc_01
implicit none

integer(c_int) :: thread_id
thread_id = omp_get_thread_num()
print *, "Hello from thread ", thread_id
if (thread_id >= 4) error stop
end subroutine

program openmp_bindc_01
use module_openmp_bindc_01
implicit none

interface
subroutine lcompilers_function() bind(c)
end subroutine
end interface

integer :: max_threads
call omp_set_num_threads(4)
max_threads = omp_get_max_threads()

if (max_threads /= 4) error stop

print *, "Max threads: ", max_threads
call GOMP_parallel(c_funloc(lcompilers_function), c_null_ptr, max_threads, 0)
end program
