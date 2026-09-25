! A module whose startup initializer nothing in Fortran calls: the program is
! written in C, so there is no Fortran main program to root the chain of
! initializer calls. The module must still be initialized by the object file
! that defines it. See https://github.com/lfortran/lfortran/issues/13387
!
! `p => tgt` is laid out as static data, the address of `tgt`, so it needs no
! startup code at all. `q => str` is currently lowered to a statement of the
! module's startup initializer, which only the defining object file's startup
! hook runs here. That is how the association is lowered today, not a limit
! of static data: the address and the length of `str` are both known at link
! time.
module global_init_08_m
    implicit none
    integer, target :: tgt = 7
    integer, pointer :: p => tgt
    character(3), target :: str = "abc"
    character(:), pointer :: q => str
contains
    subroutine check() bind(c, name="global_init_08_check")
        if (.not. associated(p, tgt)) error stop 1
        if (p /= 7) error stop 2
        p = 9
        if (tgt /= 9) error stop 3
        if (.not. associated(q)) error stop 4
        if (len(q) /= 3) error stop 5
        if (q /= "abc") error stop 6
        q = "xyz"
        if (str /= "xyz") error stop 7
        print *, "ok"
    end subroutine check
end module global_init_08_m
