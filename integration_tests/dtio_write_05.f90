! Defined output of extended types: a binding inherited from the parent type,
! a binding overridden by the extended type, and polymorphic items (an
! allocatable class(t) variable and a class(t) dummy argument) dispatched on
! their dynamic type, mixed with intrinsic items in one format.
module dtio_write_05_mod
implicit none
integer :: ncalls = 0, last_unit = -1
character(len=16) :: last_iotype = ''

type :: t
    integer :: i = 0
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type

! Inherits the binding of t.
type, extends(t) :: t2
    integer :: j = 0
end type

! Overrides the specific binding of the generic.
type, extends(t) :: t3
    integer :: k = 0
contains
    procedure :: wf => wf3
end type

! Inherits the overriding binding of t3.
type, extends(t3) :: t4
    integer :: m = 0
end type

! No defined output: written component by component, parent first.
type :: p
    integer :: a = 0
end type

type, extends(p) :: p2
    integer :: b = 0
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
    select type (dtv)
    type is (t2)
        write(unit, '("<t2:",a,":",i0,",",i0,">")', iostat=iostat) &
            iotype, dtv%i, dtv%j
    class default
        write(unit, '("<t:",a,":",i0,">")', iostat=iostat) iotype, dtv%i
    end select
end subroutine

subroutine wf3(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t3), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    ncalls = ncalls + 1
    last_unit = unit
    last_iotype = iotype
    write(unit, '("<t3:",a,":",i0,",",i0,":",i0,">")', iostat=iostat) &
        iotype, dtv%i, dtv%k, size(v_list)
end subroutine

subroutine show(u, x)
    integer, intent(in) :: u
    class(t), intent(in) :: x
    write(u, '(a,dt"cls",1x,i0)') '[', x, 9
end subroutine

end module

program dtio_write_05
use dtio_write_05_mod
implicit none
type(t) :: x
type(t2) :: y
type(t3) :: z
type(t4) :: w
type(p2) :: q
class(t), allocatable :: c
integer :: out

x%i = 1
y%i = 2
y%j = 3
z%i = 4
z%k = 5
w%i = 6
w%k = 7
w%m = 8
q%a = 10
q%b = 11
open(newunit=out, status='scratch', action='readwrite', form='formatted')

! Inherited binding, non-polymorphic item.
ncalls = 0
write(out, '(i0,1x,dt"in",1x,a)') 7, y, 'end'
if (ncalls /= 1) error stop 1
if (last_unit /= out) error stop 2
if (last_iotype /= 'DTin') error stop 3

! Overriding binding, non-polymorphic item, next to the parent type.
ncalls = 0
write(out, '(dt,1x,dt"ov"(2,3),1x,f3.1)') x, z, 2.5
if (ncalls /= 2) error stop 4
if (last_iotype /= 'DTov') error stop 5

! A binding inherited from the type that overrides it.
write(out, '(dt)') w
if (last_iotype /= 'DT') error stop 6

! Dispatch on the dynamic type of an allocatable polymorphic variable.
allocate(c, source=y)
ncalls = 0
write(out, '(a,dt"c",i3)') 'c=', c, 12
if (ncalls /= 1) error stop 7
deallocate(c)
allocate(c, source=z)
write(out, '(a,dt"c",i3)') 'c=', c, 13
if (ncalls /= 2) error stop 8
if (last_iotype /= 'DTc') error stop 9
deallocate(c)
allocate(c, source=x)
write(out, '(a,dt"c",i3)') 'c=', c, 14

! A class(t) dummy argument.
ncalls = 0
call show(out, x)
call show(out, y)
call show(out, z)
call show(out, w)
if (ncalls /= 4) error stop 10
if (last_unit /= out) error stop 11
if (last_iotype /= 'DTcls') error stop 12

! PRINT of an inherited binding: the child sees unit 6.
print '(dt"pr")', y
if (last_unit /= 6) error stop 13
if (last_iotype /= 'DTpr') error stop 14

! An extended type without defined output.
write(out, '(i0,1x,i0)') q

rewind(out)
call check('7 <t2:DTin:2,3> end')
call check('<t:DT:1> <t3:DTov:4,5:2> 2.5')
call check('<t3:DT:6,7:0>')
call check('c=<t2:DTc:2,3> 12')
call check('c=<t3:DTc:4,5:0> 13')
call check('c=<t:DTc:1> 14')
call check('[<t:DTcls:1> 9')
call check('[<t2:DTcls:2,3> 9')
call check('[<t3:DTcls:4,5:0> 9')
call check('[<t3:DTcls:6,7:0> 9')
call check('10 11')
close(out)
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
