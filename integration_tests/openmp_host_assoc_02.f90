! Worker threads call a callback made by a capturing internal procedure of a
! host that is still active on the main thread.
module openmp_host_assoc_02_m
implicit none
abstract interface
    subroutine callback(value)
        integer, intent(out) :: value
    end subroutine
    subroutine runner(values)
        integer, intent(out) :: values(2)
    end subroutine
end interface
procedure(callback), pointer :: published => null()
contains
subroutine invoke(p, value)
    procedure(callback) :: p
    integer, intent(out) :: value
    call p(value)
end subroutine

subroutine invoke_published(value)
    integer, intent(out) :: value
    call invoke(published, value)
end subroutine

subroutine publish_host(values, workers)
    integer, intent(out) :: values(2)
    procedure(runner) :: workers
    integer :: captured
    captured = 207
    published => read_state
    ! Workers invoke a callback made by this still-active host on the main thread.
    call workers(values)
    nullify(published)
contains
    subroutine read_state(value)
        integer, intent(out) :: value
        value = captured
    end subroutine
end subroutine
end module

program openmp_host_assoc_02
use omp_lib
use openmp_host_assoc_02_m, only: publish_host, invoke_published
use iso_c_binding, only: c_int
implicit none
interface
    subroutine set_dynamic(flag) bind(c, name="omp_set_dynamic")
        import :: c_int
        integer(c_int), value :: flag
    end subroutine
end interface
integer :: values(2)
call set_dynamic(0_c_int)
values = -1
call publish_host(values, workers)
print *, values
if (values(1) /= 207 .or. values(2) /= 207) error stop 1
contains
subroutine workers(values)
    integer, intent(out) :: values(2)
    integer :: id
    !$omp parallel num_threads(2) default(none) private(id) shared(values)
    id = omp_get_thread_num()
    if (omp_get_num_threads() /= 2) error stop 2
    call invoke_published(values(id + 1))
    !$omp end parallel
end subroutine
end program
