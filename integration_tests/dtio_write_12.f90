! A defined output procedure executes STOP while its parent WRITE statement
! is still in progress: the program terminates normally (exit status 0).
module dtio_write_12_m
type :: t
    integer :: a
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type
contains
subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '(i0)', iostat=iostat, iomsg=iomsg) dtv%a
    if (dtv%a == 2) stop
end subroutine
end module

program dtio_write_12
use dtio_write_12_m
type(t) :: x(2)
x = [t(1), t(2)]
write(*, '(dt,1x,dt)') x(1), x(2)
error stop "the defined output procedure did not stop the program"
end program
