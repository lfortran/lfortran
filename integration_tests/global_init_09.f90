! The program deliberately does not `use` the module that
! `global_init_09_s` uses, so nothing the program can observe reaches that
! module's startup initializer.
! See https://github.com/lfortran/lfortran/issues/13233
program global_init_09
    implicit none
    interface
        subroutine global_init_09_s()
        end subroutine global_init_09_s
    end interface
    print *, "before"
    call global_init_09_s()
    print *, "after"
end program global_init_09
