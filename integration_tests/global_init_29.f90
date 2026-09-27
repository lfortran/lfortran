! A bind(c) procedure, called from C (global_init_29c.c) once the runtime is
! started, whose dummy is of a derived type the procedure declares itself and
! whose automatic array takes its extent from module state, which the
! startup has initialized by then.
module global_init_29_m
    use iso_c_binding, only: c_int
    implicit none
    integer(c_int) :: n = 4
contains
    subroutine fill(x) bind(c, name="global_init_29_fill")
        type, bind(c) :: local_type
            integer(c_int) :: value
        end type
        type(local_type), intent(out) :: x
        integer(c_int) :: work(n)
        work = 1
        x%value = sum(work)
    end subroutine fill
end module global_init_29_m
