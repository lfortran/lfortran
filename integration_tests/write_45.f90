! A PRINT with defined output in a BLOCK of a procedure calls the
! user-defined output procedure.
module write_45_mod
implicit none
integer :: nout = 0, last = 0
type :: t
    integer :: v = 0
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
    nout = nout + 1
    last = dtv%v
    write(unit, '(i0)', iostat=iostat) dtv%v
end subroutine
subroutine show()
    type(t) :: y
    y%v = 4
    block
        print '(dt)', y
    end block
end subroutine
end module

program write_45
use write_45_mod
implicit none
call show()
print *, nout, last
if (nout /= 1) error stop 1
if (last /= 4) error stop 2
end program
