module A
implicit none

type D
    integer :: member
end type

type E
    type(D), allocatable :: ds(:)
end type

type F
    type(E), allocatable :: es(:)
end type

contains

function get_ds() result(ds)
type(D), allocatable :: ds(:)
ds = [D(1), D(2), D(3)]
end function

subroutine new_ds(self)
type(F), intent(inout) :: self

if( .not. allocated(self%es) ) error stop
self%es(2)%ds = get_ds()

end subroutine
end module

program allocate_12
use A
implicit none

type(F) :: fobj(2)

allocate(fobj(2)%es(3))
if(.not. allocated(fobj(2)%es)) error stop
if( allocated(fobj(1)%es) ) error stop

call new_ds(fobj(2))
print *, fobj(2)%es(2)%ds
if( fobj(2)%es(2)%ds(1)%member /= 1 ) error stop
if( fobj(2)%es(2)%ds(2)%member /= 2 ) error stop
if( fobj(2)%es(2)%ds(3)%member /= 3 ) error stop

! if( any(fobj(2)%es(2)%ds(:)%member /= (/1, 2, 3/)) ) error stop ! TODO: Uncomment

end program
