module optional_05_mod

logical :: opt_present, opt_alloc_present

contains

    subroutine opt(i)
    integer, optional, intent(in) :: i
    print *, 'Present in opt: ', present(i)
    opt_present = present(i)
    end subroutine opt 

    subroutine opt_alloc(i)
    integer, allocatable, optional, intent(in) :: i
    print *, 'Present in opt_alloc: ', present(i)
    opt_alloc_present = present(i)
    call opt(i)
    end subroutine opt_alloc 

end module


program optional_05
use optional_05_mod
integer, allocatable :: i
opt_present = .false.
opt_alloc_present = .false.

#ifndef __GFORTRAN__
call opt_alloc()
#endif
if (opt_alloc_present .neqv. .false.) error stop
if (opt_present .neqv. .false.) error stop

call opt_alloc(i)
if (opt_alloc_present .neqv. .true.) error stop
if (opt_present .neqv. .false.) error stop

allocate(i)
call opt_alloc(i)
if (opt_alloc_present .neqv. .true.) error stop
if (opt_present .neqv. .true.) error stop
end program
