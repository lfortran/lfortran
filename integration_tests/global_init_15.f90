! A module driven only from C (global_init_15c.c), so there is no Fortran
! main program: the deferred-length character pointer's initial target, the
! allocatable array and the null array pointer must all be set up by the
! object file that defines the module, whatever else the module holds.
! See https://github.com/lfortran/lfortran/issues/13387
module global_init_15_m
    implicit none
    character(3), target :: str = "abc"
    character(:), pointer :: q => str
    integer, allocatable :: a(:)
    integer, pointer :: pa(:) => null()
    integer, target :: tgt(3) = [4, 5, 6]
contains
    subroutine check() bind(c, name="global_init_15_check")
        if (.not. associated(q)) error stop 1
        if (len(q) /= 3) error stop 2
        if (q /= "abc") error stop 3
        q = "xyz"
        if (str /= "xyz") error stop 4
        if (allocated(a)) error stop 5
        allocate(a(2))
        a = 7
        if (sum(a) /= 14) error stop 6
        deallocate(a)
        if (associated(pa)) error stop 7
        pa => tgt
        if (sum(pa) /= 15) error stop 8
        print *, "ok"
    end subroutine check
end module global_init_15_m
