module omp_lib
use iso_c_binding, only: c_funptr, c_ptr, c_int, c_long, c_bool
implicit none

interface
subroutine GOMP_parallel (fn, data, num_threads, flags) bind (C, name="GOMP_parallel")
import :: c_funptr, c_ptr, c_int
type(c_funptr), value :: fn
type(c_ptr), value :: data
integer(c_int), value :: num_threads
integer(c_int), value :: flags
end subroutine

subroutine GOMP_task(fn, data, cpyfn, arg_size, arg_align, if_clause, flags, depend) &
                         bind(C, name="GOMP_task")
      import :: c_funptr, c_ptr, c_long, c_bool, c_int
      type(c_ptr), value :: fn, data, cpyfn, depend
      integer(c_long), value :: arg_size, arg_align
      logical(c_bool), value :: if_clause
      integer(c_int), value :: flags
end subroutine

subroutine GOMP_taskwait() bind(C, name="GOMP_taskwait")
end subroutine

subroutine gomp_teams(fn, data, num_teams, thread_limit) bind(c, name="GOMP_teams_reg")
    import :: c_funptr, c_ptr, c_int
    type(c_funptr), value :: fn
    type(c_ptr), value :: data
    integer(c_int), value :: num_teams
    integer(c_int), value :: thread_limit
end subroutine gomp_teams

function omp_get_team_num() bind(c, name="omp_get_team_num")
    import :: c_int
    integer(c_int) :: omp_get_team_num
end function omp_get_team_num

function omp_get_num_teams() bind(c, name="omp_get_num_teams")
    import :: c_int
    integer(c_int) :: omp_get_num_teams
end function omp_get_num_teams

function omp_get_team_size(level) bind(c, name="omp_get_team_size")
    import :: c_int
    integer(c_int), value :: level
    integer(c_int) :: omp_get_team_size
end function omp_get_team_size

integer(c_int) function GOMP_sections_start(count) bind(C, name="GOMP_sections_start")
    import :: c_int
    integer(c_int), value :: count
end function GOMP_sections_start

integer(c_int) function GOMP_sections_next() bind(C, name="GOMP_sections_next")
    import :: c_int
end function GOMP_sections_next

subroutine GOMP_sections_end() bind(C, name="GOMP_sections_end")
end subroutine GOMP_sections_end

function GOMP_loop_static_start(start, end, incr, chunk, istart, iend) bind(C, name="GOMP_loop_static_start")
    import :: c_long, c_bool
    integer(c_long), value :: start, end, incr, chunk
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_static_start
end function

function GOMP_loop_static_next(istart, iend) bind(C, name="GOMP_loop_static_next")
    import :: c_long, c_bool
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_static_next
end function

function GOMP_loop_dynamic_start(start, end, incr, chunk, istart, iend) bind(C, name="GOMP_loop_dynamic_start")
    import :: c_long, c_bool
    integer(c_long), value :: start, end, incr, chunk
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_dynamic_start
end function

function GOMP_loop_dynamic_next(istart, iend) bind(C, name="GOMP_loop_dynamic_next")
    import :: c_long, c_bool
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_dynamic_next
end function

function GOMP_loop_runtime_start(start, end, incr, istart, iend) bind(C, name="GOMP_loop_runtime_start")
    import :: c_long, c_bool
    integer(c_long), value :: start, end, incr
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_runtime_start
end function

function GOMP_loop_runtime_next(istart, iend) bind(C, name="GOMP_loop_runtime_next")
    import :: c_long, c_bool
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_runtime_next
end function

function GOMP_loop_guided_start(start, end, incr, chunk, istart, iend) bind(C, name="GOMP_loop_guided_start")
    import :: c_long, c_bool
    integer(c_long), value :: start, end, incr, chunk
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_guided_start
end function

function GOMP_loop_guided_next(istart, iend) bind(C, name="GOMP_loop_guided_next")
    import :: c_long, c_bool
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_guided_next
end function

function GOMP_loop_auto_start(start, end, incr, istart, iend) bind(C, name="GOMP_loop_auto_start")
    import :: c_long, c_bool
    integer(c_long), value :: start, end, incr
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_auto_start
end function

function GOMP_loop_auto_next(istart, iend) bind(C, name="GOMP_loop_auto_next")
    import :: c_long, c_bool
    integer(c_long) :: istart, iend
    logical(c_bool) :: GOMP_loop_auto_next
end function

! ---------------------- LOOP END ----------------------
subroutine GOMP_loop_end() bind(C, name="GOMP_loop_end")
end subroutine GOMP_loop_end

subroutine GOMP_loop_end_nowait() bind(C, name="GOMP_loop_end_nowait")
end subroutine GOMP_loop_end_nowait


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

function omp_get_num_threads() bind(c, name="omp_get_num_threads")
import :: c_int
integer(c_int) :: omp_get_num_threads
end function omp_get_num_threads

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
