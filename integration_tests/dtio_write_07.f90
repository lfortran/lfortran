module dtio_write_07_m
implicit none
type :: t
    integer :: v = 0
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type
type :: pair
    type(t) :: a, b
contains
    procedure :: wfp
    generic :: write(formatted) => wfp
end type
type :: u
    integer :: v = 0
contains
    procedure :: wfu
    generic :: write(formatted) => wfu
end type
integer :: last_unit = 0, ncalls = 0
contains
subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    last_unit = unit
    ncalls = ncalls + 1
    write(unit, '(a,":",i0)', iostat=iostat) iotype, dtv%v
end subroutine
subroutine wfp(dtv, unit, iotype, v_list, iostat, iomsg)
    class(pair), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '("[",dt,"|",dt,"]")', iostat=iostat) dtv%a, dtv%b
end subroutine
subroutine wfu(dtv, unit, iotype, v_list, iostat, iomsg)
    class(u), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '(i0)', iostat=iostat) dtv%v
end subroutine
end module

program dtio_write_07
use dtio_write_07_m
implicit none
type(t) :: y, z
type(pair) :: pp
character(20) :: s
character(12) :: arr(3)
character(len=:), allocatable :: d

y%v = 1
z%v = 2

! Character scalar internal file; the child sees a negative unit.
last_unit = 0
ncalls = 0
write(s, '(dt,1x,dt)') y, z
if (s /= 'DT:1 DT:2') error stop 1
if (ncalls /= 2) error stop 2
if (last_unit >= 0) error stop 3

! iotype suffix and v_list, followed by an intrinsic item.
write(s, '(dt"x"(3),1x,i0)') y, 5
if (s /= 'DTx:1 5') error stop 4

! Character array internal file: `/` moves to the next record and the
! remaining records are blank filled.
arr = '#'
write(arr, '(dt,/,a,1x,dt)') y, 'mid', z
if (arr(1) /= 'DT:1') error stop 5
if (arr(2) /= 'mid DT:2') error stop 6
if (arr(3) /= '#') error stop 7

! Deferred-length allocatable character internal file.
allocate(character(15) :: d)
write(d, '(i0,1x,dt)') 7, z
if (len(d) /= 15) error stop 8
if (d /= '7 DT:2') error stop 9

! List-directed output to an internal file.
last_unit = 0
write(s, *) y, 'z'
if (adjustl(s) /= 'LISTDIRECTED:1 z') error stop 10
if (last_unit >= 0) error stop 11

! Nested DT output in a child whose parent writes to an internal file.
pp%a%v = 3
pp%b%v = 4
write(s, '(a,dt,a)') '<', pp, '>'
if (s /= '<[DT:3|DT:4]>') error stop 12

! Parent TL after the child moves back into the child's output.
write(s, '(dt,tl3,a)') y, 'Q'
if (s /= 'DQ:1') error stop 13

! Issue #13768.
call check_13768()

print *, 'ok'

contains

subroutine check_13768()
    type(u) :: w
    character(20) :: s1
    w%v = 42
    write(s1, '(dt)') w
    if (trim(s1) /= '42') error stop 14
end subroutine

end program
