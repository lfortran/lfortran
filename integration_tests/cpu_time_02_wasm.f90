program cpu_time_02_wasm
use iso_fortran_env, only: dp=>real64
implicit none
real(dp) :: t1, t2
interface
    subroutine cpu_time(t) bind(c)
        import :: dp
        real(dp), intent(out) :: t
    end subroutine
end interface
call cpu_time(t1)
print *, "Some computation"
call cpu_time(t2)
print *, "Total time: ", t2-t1
end program
