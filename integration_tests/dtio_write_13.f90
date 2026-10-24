! A nested defined output procedure executes STOP while two parent WRITE
! statements are in progress, the outermost to an internal file: the program
! terminates normally (exit status 0).
module dtio_write_13_m
type :: inner
    integer :: a
contains
    procedure :: wi
    generic :: write(formatted) => wi
end type
type :: outer
    type(inner) :: x
contains
    procedure :: wo
    generic :: write(formatted) => wo
end type
contains
subroutine wi(dtv, unit, iotype, v_list, iostat, iomsg)
    class(inner), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '(i0)', iostat=iostat, iomsg=iomsg) dtv%a
    stop
end subroutine
subroutine wo(dtv, unit, iotype, v_list, iostat, iomsg)
    class(outer), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '(a,dt,a)', iostat=iostat, iomsg=iomsg) '[', dtv%x, ']'
end subroutine
end module

program dtio_write_13
use dtio_write_13_m
type(outer) :: y
character(len=40) :: buf
y = outer(inner(7))
write(buf, '(a,dt)') 'y = ', y
error stop "the defined output procedure did not stop the program"
end program
