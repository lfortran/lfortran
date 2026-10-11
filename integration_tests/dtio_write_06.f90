! List-directed output of derived-type items with defined output: the
! procedure is called with iotype "LISTDIRECTED" and an empty v_list, and the
! item is separated from its neighbors like a non-character item. Registered
! with --std=f23, so that list-directed records start with a blank as in
! GFortran.
module dtio_write_06_mod
implicit none
integer :: ncalls = 0, last_unit = -1, last_nv = -1
character(len=16) :: last_iotype = ''

type :: t
    integer :: i = 0
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type

type, extends(t) :: t2
    integer :: j = 0
end type

! The child itself writes list-directed output.
type :: u
    logical :: flag = .false.
contains
    procedure :: wfu
    generic :: write(formatted) => wfu
end type

! The child writes nothing.
type :: e
    integer :: k = 0
contains
    procedure :: wfe
    generic :: write(formatted) => wfe
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
    last_unit = unit
    last_iotype = iotype
    last_nv = size(v_list)
    select type (dtv)
    type is (t2)
        write(unit, '("<t2:",a,":",i0,",",i0,">")', iostat=iostat) &
            iotype, dtv%i, dtv%j
    class default
        write(unit, '("<",a,":",i0,":",i0,">")', iostat=iostat) &
            iotype, dtv%i, size(v_list)
    end select
end subroutine

subroutine wfu(dtv, unit, iotype, v_list, iostat, iomsg)
    class(u), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    ncalls = ncalls + 1
    write(unit, *, iostat=iostat) 'u', dtv%flag
end subroutine

subroutine wfe(dtv, unit, iotype, v_list, iostat, iomsg)
    class(e), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    ncalls = ncalls + 1
    iostat = 0
end subroutine

end module

program dtio_write_06
use dtio_write_06_mod
implicit none
type(t) :: x, a(3)
type(t2) :: y
type(u) :: v
type(e) :: z
class(t), allocatable :: c
integer :: out, i, nunit

x%i = 7
a%i = [1, 2, 3]
y%i = 4
y%j = 5
v%flag = .true.
open(newunit=out, status='scratch', action='readwrite', form='formatted')

! PRINT and WRITE to unit 6: the child sees unit 6, iotype LISTDIRECTED and
! an empty v_list.
ncalls = 0
print *, x
if (ncalls /= 1) error stop 1
if (last_unit /= 6) error stop 2
if (last_iotype /= 'LISTDIRECTED') error stop 3
if (last_nv /= 0) error stop 4
last_unit = -1
write(*, *) x, 'z'
if (ncalls /= 2) error stop 5
if (last_unit /= 6) error stop 6
print *, 1, x, 2
if (ncalls /= 3) error stop 7
print *, a
if (ncalls /= 6) error stop 8

! An external unit, read back.
ncalls = 0
write(out, *) x
if (last_unit /= out) error stop 9
if (last_iotype /= 'LISTDIRECTED') error stop 10
write(out, *) x, 'z'
write(out, *) 1, x, 2
write(out, *) a
write(out, *) (a(i), i = 1, 3)
write(out, *) 'a', x, 'b'
write(out, *) x, x
write(out, *) '[', (a(i), i = 1, 2), ']'
if (ncalls /= 14) error stop 11

! A polymorphic scalar, dispatched on its dynamic type.
allocate(c, source=y)
write(out, *) 'c', c
deallocate(c)
allocate(c, source=x)
write(out, *) c, 'c'

! A child that writes list-directed output, in a list-directed and in a
! formatted parent.
write(out, *) '[', v, ']'
write(out, '(a,dt,a)') '[', v, ']'

! A child that writes nothing is still separated from the next item.
write(out, *) z, 'b'
write(out, *) 'a', z, 'b'

! A unit given by an expression is evaluated once.
nunit = 0
write(next_unit(), *) x
if (nunit /= 1) error stop 12
if (last_unit /= out) error stop 13

rewind(out)
call check(' <LISTDIRECTED:7:0>')
call check(' <LISTDIRECTED:7:0> z')
call check_squeezed(' 1 <LISTDIRECTED:7:0> 2')
call check(' <LISTDIRECTED:1:0> <LISTDIRECTED:2:0> <LISTDIRECTED:3:0>')
call check(' <LISTDIRECTED:1:0> <LISTDIRECTED:2:0> <LISTDIRECTED:3:0>')
call check(' a <LISTDIRECTED:7:0> b')
call check(' <LISTDIRECTED:7:0> <LISTDIRECTED:7:0>')
call check(' [ <LISTDIRECTED:1:0> <LISTDIRECTED:2:0> ]')
call check(' c <t2:LISTDIRECTED:4,5>')
call check(' <LISTDIRECTED:7:0> c')
call check(' [  u T ]')
call check('[ u T]')
call check('  b')
call check(' a  b')
call check(' <LISTDIRECTED:7:0>')
close(out)
print *, 'ok'

contains

integer function next_unit()
    nunit = nunit + 1
    next_unit = out
end function

subroutine check(expected)
    character(*), intent(in) :: expected
    character(len=80) :: line
    read(out, '(a)') line
    if (trim(line) /= expected) then
        print *, '"', trim(line), '" /= "', expected, '"'
        error stop 30
    end if
end subroutine

! Compares with runs of blanks reduced to one blank: the width of a
! list-directed integer is processor dependent.
subroutine check_squeezed(expected)
    character(*), intent(in) :: expected
    character(len=80) :: line, squeezed
    integer :: k, n
    read(out, '(a)') line
    squeezed = ''
    n = 0
    do k = 1, len_trim(line)
        if (line(k:k) == ' ' .and. n > 0) then
            if (squeezed(n:n) == ' ') cycle
        end if
        n = n + 1
        squeezed(n:n) = line(k:k)
    end do
    if (trim(squeezed) /= expected) then
        print *, '"', trim(line), '" /= "', expected, '"'
        error stop 31
    end if
end subroutine

end program
