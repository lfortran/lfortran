! A module whose startup initializer nothing in Fortran calls: the program is
! written in C, so there is no Fortran main program to root the chain of
! initializer calls. The module must still be initialized by the object file
! that defines it. See https://github.com/lfortran/lfortran/issues/13387
module global_init_08_m
    implicit none
    integer, target :: tgt = 7
    integer, pointer :: p => tgt
contains
    subroutine check() bind(c, name="global_init_08_check")
        if (.not. associated(p)) error stop 1
        if (p /= 7) error stop 2
        p = 9
        if (tgt /= 9) error stop 3
        print *, "ok"
    end subroutine check
end module global_init_08_m
