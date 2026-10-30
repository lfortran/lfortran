module openmp_host_assoc_02_m
use omp_lib
implicit none
abstract interface
    subroutine callback(value)
        integer, intent(out) :: value
    end subroutine
end interface
procedure(callback), pointer :: published => null()
contains
subroutine invoke(p, value)
    procedure(callback) :: p
    integer, intent(out) :: value
    call p(value)
end subroutine

recursive subroutine concurrent_host(id, value)
    integer, intent(in) :: id
    integer, intent(out) :: value
    integer :: captured
    procedure(callback), pointer :: p
    captured = 100 + id
    ! Both hosts stay alive; publish thread 1's callback last.
    if (id == 0) p => read_state
    !$omp barrier
    if (id == 1) p => read_state
    !$omp barrier
    call p(value)
    !$omp barrier
contains
    subroutine read_state(value)
        integer, intent(out) :: value
        value = captured
    end subroutine
end subroutine

subroutine invoke_published(value)
    integer, intent(out) :: value
    call invoke(published, value)
end subroutine

subroutine workers(values)
    integer, intent(out) :: values(2)
    integer :: id
    !$omp parallel num_threads(2) default(none) private(id) shared(values)
    id = omp_get_thread_num()
    call invoke_published(values(id + 1))
    !$omp end parallel
end subroutine

subroutine publish_host(values)
    integer, intent(out) :: values(2)
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
use openmp_host_assoc_02_m
use iso_c_binding, only: c_int
implicit none
interface
    subroutine set_dynamic(flag) bind(c, name="omp_set_dynamic")
        import :: c_int
        integer(c_int), value :: flag
    end subroutine
end interface
integer :: id, values(2), worker_values(2)
call set_dynamic(0_c_int)
values = -1
!$omp parallel num_threads(2) default(none) private(id) shared(values)
id = omp_get_thread_num()
if (omp_get_num_threads() /= 2) error stop 1
call concurrent_host(id, values(id + 1))
!$omp end parallel
if (any(values /= [100, 101])) error stop 2
worker_values = -1
call publish_host(worker_values)
if (any(worker_values /= [207, 207])) error stop 3
end program
