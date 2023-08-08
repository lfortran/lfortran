module A
implicit none

contains

function dg()
    real(kind=8), allocatable :: dg(:)
    integer :: i, ierr

    allocate(dg(10))
    dg = 10.0_8
end function dg

function rg()
    real, allocatable :: rg(:)
    allocate(rg(10)) ! TODO: Should happen automatically
    rg = real(dg())
end function rg

end module

program allocate_11
use A
implicit none

real, allocatable :: rg_result(:)
rg_result = rg()
print *, rg_result
if( any(rg_result /= 10) ) error stop

end program
