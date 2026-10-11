! Defined output with the DT edit descriptor in FORMAT statements: plain DT,
! a character literal, a v-list, a repeat count, blanks, labels before and
! after the statement, and a FORMAT statement in a module procedure.
module dtio_write_04_mod
implicit none
integer :: ncalls = 0, last_n = -1
integer :: last_v(4) = 0
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

subroutine in_module(out, x)
    integer, intent(in) :: out
    type(t), intent(in) :: x
    write(out, 10) x
10  format("m",dt'mod'(6))
end subroutine

end module

program dtio_write_04
use dtio_write_04_mod
implicit none
type(t) :: x, y
integer :: out
10 format(dt,1x,dt)

x%v = 1
y%v = 2
open(newunit=out, status='scratch', action='readwrite', form='formatted')

write(out, 10) x, y
if (last_iotype /= 'DT') error stop 1
if (last_n /= 0) error stop 2

write(out, 20) x
if (last_iotype /= 'DTx') error stop 3
if (last_n /= 1) error stop 4
if (last_v(1) /= 2) error stop 5

write(out, 30) 5, y
write(out, 40) x
if (last_iotype /= 'DTab') error stop 6
if (any(last_v(1:2) /= [1, 2])) error stop 7

ncalls = 0
write(out, 50) x, y, 9
if (ncalls /= 2) error stop 8
if (last_v(1) /= 3) error stop 9

write(out, 60) x
if (last_iotype /= "DTit's") error stop 10
if (any(last_v(1:2) /= [1, 2])) error stop 11

write(out, 70) x
if (last_iotype /= 'DTq"r') error stop 12

write(out, 80) x, y
call in_module(out, y)
if (last_iotype /= 'DTmod') error stop 13

ncalls = 0
print 10, x, y
if (ncalls /= 2) error stop 14
print 20, y
if (last_iotype /= 'DTx') error stop 15

20 format(dt'x'(2))
30 format(i0,1x,dt)
40 format(dt"ab"(1,2))
50 format(2dt(3),1x,i0)
60 format(DT 'it''s' ( 1 , 2 ))
70 format(dt"q""r")
80 format(1x,2(dt'k'(4),:,"|"))

rewind(out)
call check('<DT:1: <DT:2:')
call check('<DTx:1:2')
call check('5 <DT:2:')
call check('<DTab:1:1,2')
call check('<DT:1:3<DT:2:3 9')
call check("<DTit's:1:1,2")
call check('<DTq"r:1:')
call check(' <DTk:1:4|<DTk:2:4')
call check('m<DTmod:2:6')
close(out)
print *, 'ok'

contains

subroutine check(expected)
    character(*), intent(in) :: expected
    character(len=64) :: line
    read(out, '(a)') line
    if (trim(line) /= expected) then
        print *, '"', trim(line), '" /= "', expected, '"'
        error stop 20
    end if
end subroutine

end program
