! A module whose pointers are initially associated with parts of another
! module's variable, both reached only through a separately compiled
! external subroutine. Whatever storage the target's module sets up at
! startup has to be there before the pointers' module associates with it,
! whichever order the object files are linked in: here the target's module
! comes first, global_init_17 links the other way round.
program global_init_16
    implicit none
    interface
        subroutine global_init_16_s()
        end subroutine global_init_16_s
    end interface
    call global_init_16_s()
end program global_init_16
