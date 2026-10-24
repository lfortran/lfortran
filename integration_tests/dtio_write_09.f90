! A defined output procedure returns a nonzero iostat and the parent has no
! iostat=: error termination (issue #14521). Registered with FAIL.
module dtio_write_09_m
type :: e
contains
procedure :: wf
generic :: write(formatted) => wf
end type
contains
subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
    class(e), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    iostat = 5
    iomsg = 'boom'
end subroutine
end module

program dtio_write_09
use dtio_write_09_m
type(e) :: x
print "(dt)", x
print *, 'should not reach here (error termination expected)'
end program
