! Module array pointers initially associated with a whole array and with an
! array section, in a module only a separately compiled external subroutine
! uses. Their descriptors belong to the module and have to describe the
! targets before the first use, although the program cannot see the module.
! GFortran 16.1 lays these pointers out as zeros, dropping the initial
! targets even when a program uses the module directly, so this test is not
! labelled gfortran.
program global_init_19
    implicit none
    interface
        subroutine global_init_19_s()
        end subroutine global_init_19_s
    end interface
    call global_init_19_s()
end program global_init_19
