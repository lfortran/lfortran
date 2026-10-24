! Defined output: the DT edit descriptor passes its character literal as the
! iotype suffix (doubled quotes unescaped) and its v-list as v_list, for
! constant and character-variable formats, with a repeat count.
module dtio_write_03_mod
implicit none
integer :: ncalls = 0, last_n = -1
integer :: last_v(8) = 0
character(len=16) :: last_iotype = ''

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
    ncalls = ncalls + 1
    last_iotype = iotype
    last_n = size(v_list)
    last_v = 0
    last_v(1:size(v_list)) = v_list
    write(unit, '("<",a,":",i0,":",*(i0,:,","))', iostat=iostat) &
        iotype, dtv%v, v_list
end subroutine

end module

program dtio_write_03
use dtio_write_03_mod
implicit none
type(t) :: x, y
character(len=:), allocatable :: f
integer :: out, ios, out2

x%v = 7
y%v = 8
open(newunit=out, status='scratch', action='readwrite', form='formatted')
open(newunit=out2, status='scratch', action='readwrite', form='formatted')

write(out, '(dt(5))') x
if (last_iotype /= 'DT') error stop 1
if (last_n /= 1) error stop 2
if (last_v(1) /= 5) error stop 3

write(out, '(dt"ab"(3,4))') x
if (last_iotype /= 'DTab') error stop 4
if (last_n /= 2) error stop 5
if (any(last_v(1:2) /= [3, 4])) error stop 6

write(out, "(dt'x''y'(1,20,7))") x
if (last_iotype /= "DTx'y") error stop 7
if (last_n /= 3) error stop 8
if (any(last_v(1:3) /= [1, 20, 7])) error stop 9

f = '(dt"rt"(12,3),1x,dt)'
write(out, f) x, y
if (last_iotype /= 'DT') error stop 10
if (last_n /= 0) error stop 11

ncalls = 0
write(out, '(2dt(3),1x,i0)') x, y, 9
if (ncalls /= 2) error stop 12
if (last_iotype /= 'DT') error stop 13
if (last_n /= 1) error stop 14
if (last_v(1) /= 3) error stop 15

write(out, '(dt"tag"(10,3))') x
if (last_iotype /= 'DTtag') error stop 16

print '(dt"p"(1,2))', x
if (last_iotype /= 'DTp') error stop 17
if (last_n /= 2) error stop 18

! The standard allows signed v-list values; GFortran rejects zero and negative
! values with a run-time format error, so check them only if the child ran.
ncalls = 0
f = '(dt"q""r"(-1,0,+7))'
write(out2, f, iostat=ios) x
if (ncalls > 0) then
    if (ios /= 0) error stop 19
    if (last_iotype /= 'DTq"r') error stop 20
    if (last_n /= 3) error stop 21
    if (any(last_v(1:3) /= [-1, 0, 7])) error stop 22
end if

rewind(out)
call check('<DT:7:5')
call check('<DTab:7:3,4')
call check("<DTx'y:7:1,20,7")
call check('<DTrt:7:12,3 <DT:8:')
call check('<DT:7:3<DT:8:3 9')
call check('<DTtag:7:10,3')
close(out)
close(out2)
print *, 'ok'

contains

subroutine check(expected)
    character(*), intent(in) :: expected
    character(len=64) :: line
    read(out, '(a)') line
    if (trim(line) /= expected) then
        print *, '"', trim(line), '" /= "', expected, '"'
        error stop 30
    end if
end subroutine

end program
