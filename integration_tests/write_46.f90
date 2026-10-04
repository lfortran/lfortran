! Defined input/output of a pointer item calls the user-defined procedure
! with its target.
module write_46_mod
implicit none
integer :: nout = 0, last = 0
type :: t
    integer :: v = 0
contains
    procedure :: wf
    procedure :: rf
    generic :: write(formatted) => wf
    generic :: read(formatted) => rf
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

subroutine rf(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    read(unit, '(i1)', iostat=iostat) dtv%v
    dtv%v = dtv%v + 100
end subroutine
end module

program write_46
use write_46_mod
implicit none
type(t), target :: x
type(t), pointer :: p
integer :: u
x%v = 4
p => x
print '(dt)', p
if (nout /= 1) error stop 1
if (last /= 4) error stop 2
p%v = 5
write(*, '(dt)') p
if (nout /= 2) error stop 3
if (last /= 5) error stop 4
open(newunit=u, status='scratch')
write(u, '(a)') '7'
rewind(u)
read(u, '(dt)') p
close(u)
if (x%v /= 107) error stop 5
end program
