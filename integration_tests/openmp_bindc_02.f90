module module_openmp_bindc_02
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

subroutine GOMP_atomic_start() bind(C, name="GOMP_atomic_start")
end subroutine

subroutine GOMP_atomic_end() bind(C, name="GOMP_atomic_end")
end subroutine

end interface

end module

module thread_data_module
    use, intrinsic :: iso_c_binding
    type, bind(C) :: thread_data
        integer(c_int) :: n, ctr
    end type thread_data
end module thread_data_module

subroutine lcompilers_increment_ctr(data) bind(C)
use thread_data_module
use iso_c_binding
use module_openmp_bindc_02
implicit none
type(c_ptr), value :: data
type(thread_data), pointer :: tdata
integer(c_int) :: n, local_ctr

integer(c_int) :: i, num_threads, chunk, leftovers, thread_num, start, end

call c_f_pointer(data, tdata)

n = tdata%n

num_threads = omp_get_max_threads()
chunk = n / num_threads
leftovers = mod(n, num_threads)

thread_num = omp_get_thread_num()
start = chunk * thread_num

if (thread_num < leftovers) then
    start = start + thread_num
else
    start = start + leftovers
end if

end = start + chunk

if (thread_num < leftovers) then
    end = end + 1
end if

local_ctr = 0

do i = start + 1, end
    local_ctr = local_ctr + 1
end do

call GOMP_barrier()

call GOMP_atomic_start()
tdata%ctr = tdata%ctr + local_ctr
call GOMP_atomic_end()

end subroutine

subroutine increment_ctr(n, ctr)
use thread_data_module
use module_openmp_bindc_02
implicit none

integer(c_int), intent(in) :: n
integer(c_int), intent(inout) :: ctr

type(thread_data), target :: data
type(c_ptr) :: tdata

interface
subroutine lcompilers_increment_ctr(data) bind(C)
use iso_c_binding
type(c_ptr), value :: data
end subroutine
end interface

data%n = n
data%ctr = ctr

tdata = c_loc(data)

call GOMP_parallel(c_funloc(lcompilers_increment_ctr), tdata, 0, 0)

ctr = data%ctr

end subroutine

program openmp_bindc_02
use module_openmp_bindc_02
use thread_data_module
implicit none

integer(c_int) :: n = 1000000, ctr

call omp_set_num_threads(4)

ctr = 0
call increment_ctr(n, ctr)

print *, ctr
if (ctr /= 1000000) error stop

end program
