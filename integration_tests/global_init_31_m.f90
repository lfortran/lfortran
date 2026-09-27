! A module bind(c) procedure whose dummy is of an interoperable type the
! procedure declares itself, with an automatic array whose extent comes from
! module state and an internal procedure that reads that array by host
! association. The module state is a default initialization that needs code
! at startup, which the program's startup runs before the extent is
! evaluated. The program calls the procedure through its binding label,
! with a type of its own that is the same type because both are
! interoperable with the same components. The `fortran` backend cannot
! compile a program against a module compiled on its own, so the Fortran it
! prints is checked by global_init_30 and global_init_32 instead.
module global_init_31_m
    use iso_c_binding, only: c_int
    implicit none
    type :: config
        integer :: count = 3
    end type
    type(config) :: settings(2)
contains
    subroutine p(x) bind(c, name="global_init_31_p")
        type, bind(c) :: loc
            integer(c_int) :: value
        end type
        type(loc), intent(inout) :: x
        integer(c_int) :: work(settings(1)%count)
        work = x%value
        x%value = helper()
    contains
        integer(c_int) function helper()
            helper = sum(work) + size(work)
        end function helper
    end subroutine p
end module global_init_31_m
