! The #13233 case with a module-level allocatable array added to the module:
! the program does not use global_init_14_m, which only a separately compiled
! external subroutine reaches, so the program cannot initialize it. Neither
! the pointer component's null default nor the allocatable array's
! descriptor may depend on the program knowing about the module.
! See https://github.com/lfortran/lfortran/issues/13233
program global_init_14
    implicit none
    interface
        subroutine global_init_14_s()
        end subroutine global_init_14_s
    end interface
    call global_init_14_s()
end program global_init_14
