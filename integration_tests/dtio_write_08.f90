module dtio_write_08_m
implicit none
type :: t
    integer :: v = 0
    integer :: err = 0
    character(10) :: msg = ''
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type
type :: outer
    type(t) :: inner
contains
    procedure :: wfo
    generic :: write(formatted) => wfo
end type
integer :: ncalls = 0
contains
! Writes `X<v>`, then fails with `err` and `msg` if `err` is nonzero.
subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    ncalls = ncalls + 1
    write(unit, '(a,i0)', iostat=iostat) 'X', dtv%v
    if (dtv%err /= 0) then
        iostat = dtv%err
        iomsg = dtv%msg
    end if
end subroutine
! A child that passes on the error of its own child.
subroutine wfo(dtv, unit, iotype, v_list, iostat, iomsg)
    class(outer), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("{",dt,"}")', iostat=iostat, iomsg=iomsg) dtv%inner
end subroutine
end module

program dtio_write_08
use dtio_write_08_m
implicit none
type(t) :: ok, bad, neg
type(outer) :: o
integer :: ios, u
character(40) :: msg, line, s, arr(2)

ok%v = 1
bad%v = 2
bad%err = 5
bad%msg = 'boom'
neg%v = 4
neg%err = -1
neg%msg = 'neg'
o%inner = bad

open(newunit=u, status='scratch', form='formatted')

! Formatted parent: iostat= and iomsg= receive the child's values. The
! items after the failing one are not written by LFortran; GFortran writes
! them (the standard leaves the record indeterminate after an error).
msg = 'unset'
ncalls = 0
write(u, '(a,dt,a,dt,a)', iostat=ios, iomsg=msg) 'a', ok, 'b', bad, 'c'
if (ios /= 5) error stop 1
if (msg /= 'boom') error stop 2
if (ncalls /= 2) error stop 3

! List-directed parent.
msg = 'unset'
ncalls = 0
write(u, *, iostat=ios, iomsg=msg) 'a', ok, 'b', bad, 'c'
if (ios /= 5) error stop 4
if (msg /= 'boom') error stop 5
if (ncalls /= 2) error stop 6

! A negative iostat is passed on as is.
msg = 'unset'
write(u, '(a,dt,a)', iostat=ios, iomsg=msg) 'a', neg, 'c'
if (ios /= -1) error stop 7
if (msg /= 'neg') error stop 8

! iostat= without iomsg=, then a statement with iomsg= and no error: its
! iomsg is not changed.
write(u, '(a,dt,a)', iostat=ios) 'a', bad, 'c'
if (ios /= 5) error stop 9
msg = 'unset'
write(u, '(a)', iostat=ios, iomsg=msg) 'next'
if (ios /= 0) error stop 10
if (msg /= 'unset') error stop 11

! Nested: the child passes on the error of its own child.
msg = 'unset'
write(u, '(a,dt,a)', iostat=ios, iomsg=msg) '<', o, '>'
if (ios /= 5) error stop 12
if (msg /= 'boom') error stop 13

! The partial records were written, each ended as usual.
rewind(u)
read(u, '(a)') line
if (line /= 'aX1bX2' .and. line /= 'aX1bX2c') error stop 14
read(u, '(a)') line
line = adjustl(line)
if (line(1:6) /= 'a X1 b') error stop 15
if (index(line, 'X2') == 0) error stop 16
read(u, '(a)') line
if (line /= 'aX4' .and. line /= 'aX4c') error stop 17
read(u, '(a)') line
if (line /= 'aX2' .and. line /= 'aX2c') error stop 18
read(u, '(a)') line
if (line /= 'next') error stop 19
read(u, '(a)') line
if (line /= '<{X2' .and. line /= '<{X2}>') error stop 20
close(u)

! Internal file parent, formatted.
s = 'zzz'
msg = 'unset'
write(s, '(a,dt,a)', iostat=ios, iomsg=msg) 'a', bad, 'c'
if (ios /= 5) error stop 21
if (msg /= 'boom') error stop 22
if (s /= 'aX2' .and. s /= 'aX2c') error stop 23

! Internal file parent, list-directed.
s = 'zzz'
msg = 'unset'
write(s, *, iostat=ios, iomsg=msg) 'a', bad, 'c'
if (ios /= 5) error stop 24
if (msg /= 'boom') error stop 25
if (adjustl(s) /= 'a X2' .and. adjustl(s) /= 'a X2 c') error stop 26

! Character array internal file: the error is in the second record.
arr = 'zzz'
msg = 'unset'
write(arr, '(a,/,a,dt,a)', iostat=ios, iomsg=msg) 'r1', 'a', bad, 'c'
if (ios /= 5) error stop 27
if (msg /= 'boom') error stop 28
if (arr(1) /= 'r1') error stop 29
if (arr(2) /= 'aX2' .and. arr(2) /= 'aX2c') error stop 30

print *, 'ok'
end program
