module rand_mod_intrinsic_358
implicit none
contains
subroutine getseed(seed)
integer, intent(out) :: seed(:)
call random_seed(get=seed)
end subroutine getseed

function rand0() result(x)
use iso_fortran_env, only: RP => REAL64
real(RP) :: x
call random_number(harvest=x)
end function rand0

end module rand_mod_intrinsic_358

program main
use rand_mod_intrinsic_358
implicit none
integer :: seed(8)

call getseed(seed)
call random_seed(put=seed)
print *, rand0()

end program
