! Test defined output (DT) with multiple and mixed items in formatted I/O (Issue #13766)
module write_50_mod
implicit none
integer :: nout = 0
type :: t
    integer :: v = 0
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type
type :: err_t
    integer :: code = 0
contains
    procedure :: wf_err
    generic :: write(formatted) => wf_err
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
    write(unit, '(i0)', iostat=iostat) dtv%v
end subroutine
subroutine wf_err(dtv, unit, iotype, v_list, iostat, iomsg)
    class(err_t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    iostat = dtv%code
    iomsg = 'expected dt error'
end subroutine
end module

program write_50
use write_50_mod
implicit none
type(t) :: y, z
type(err_t) :: ey
integer :: u, stat
character(len=100) :: line
character(len=80) :: msg

y%v = 1
z%v = 2

! Check stdout formatting
print '(dt,1x,dt)', y, z
print '(a,dt)', 'x', y
write(*, '(i0,1x,dt)') 5, z
print '(dt,1x,i0)', y, 5
print '(2dt)', y, z
print '(2(1x, dt))', y, z
print '(dt, :, ", suffix")', y
print '(dt, '' it''''s '')', y

! Check file I/O formatting and verify exact values
open(newunit=u, file='write_50_tmp.txt', status='replace')

! Case 1: multiple DT
write(u, '(dt,1x,dt)') y, z

! Case 2: character then DT
write(u, '(a,dt)') 'x', y

! Case 3: integer then DT
write(u, '(i0,1x,dt)') 5, z

! Case 4: DT then integer
write(u, '(dt,1x,i0)') y, 5

! Case 5: repeat count on DT
write(u, '(2dt)') y, z

! Case 6: repeated group with DT
write(u, '(2(1x, dt))') y, z

! Case 7: colon termination when items exhausted
write(u, '(dt, :, ", suffix")') y

! Case 8: escaped single quotes in format literal
write(u, '(dt, '' it''''s '')') y

close(u)

open(newunit=u, file='write_50_tmp.txt', status='old')

read(u, '(a)') line
if (trim(line) /= '1 2') error stop 1

read(u, '(a)') line
if (trim(line) /= 'x1') error stop 2

read(u, '(a)') line
if (trim(line) /= '5 2') error stop 3

read(u, '(a)') line
if (trim(line) /= '1 5') error stop 4

read(u, '(a)') line
if (trim(line) /= '12') error stop 5

read(u, '(a)') line
if (trim(line) /= ' 1 2') error stop 6

read(u, '(a)') line
if (trim(line) /= '1') error stop 7

read(u, '(a)') line
if (trim(line) /= "1 it's") error stop 8

close(u, status='delete')

! Case 9: DT error status preservation across fragments
ey%code = 42
msg = 'initial'
open(newunit=u, file='write_50_tmp.txt', status='replace')
write(u, '(dt,1x,i0)', iostat=stat, iomsg=msg) ey, 5
close(u, status='delete')
if (stat /= 42) error stop 10
if (trim(msg) /= 'expected dt error') error stop 11

! Case 10: labeled colon write
open(newunit=u, status='scratch')
goto 100
error stop 12
100 write(u, '(:,dt)') y
close(u)

if (nout /= 23) error stop 13

end program
