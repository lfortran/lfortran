program arrays_33
implicit none

type d
    real(8), dimension(:), pointer :: r
end type

type(d) :: objd
real(8) :: array(10)
array = 24.0
call f(array, objd)
objd%r => array ! TODO: Remove
array = 25.0
print *, objd%r
if( any(objd%r /= 25.0) ) error stop

contains

subroutine f(array, objd)
real(8), intent(out), target :: array(:)
type(d), intent(out) :: objd
real(8), target :: array1(size(array))
objd%r => array1
array1 = array
if( any(objd%r /= 24.0) ) error stop
! objd%r => array TODO: Uncomment
end subroutine

end program
