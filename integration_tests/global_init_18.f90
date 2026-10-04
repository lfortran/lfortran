! A module the program does not use, reached through two separately compiled
! external subroutines: the first changes every kind of storage the module
! has -- static data, a string buffer, allocatable arrays of the module and
! of a component -- and the second checks that the changes are still there.
! The module is initialized once, before the first use, and never again.
program global_init_18
    implicit none
    interface
        subroutine global_init_18_set()
        end subroutine global_init_18_set
        subroutine global_init_18_check()
        end subroutine global_init_18_check
    end interface
    call global_init_18_set()
    call global_init_18_check()
end program global_init_18
