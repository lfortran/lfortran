! Defined output of extended types declared in the main program and in an
! internal procedure, outside a module: bindings inherited from a module type
! and from a module type that overrides them, for non-polymorphic items and
! for polymorphic items dispatched on their dynamic type.
module dtio_write_15_mod
implicit none
integer :: ncalls = 0

type :: t
    integer :: i = 0
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type

! Overrides the binding of t in the module.
type, extends(t) :: t3
    integer :: k = 0
contains
    procedure :: wf => wf3
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
    write(unit, '("<t:",a,":",i0,">")', iostat=iostat) iotype, dtv%i
end subroutine

subroutine wf3(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t3), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    ncalls = ncalls + 1
    write(unit, '("<t3:",a,":",i0,",",i0,">")', iostat=iostat) &
        iotype, dtv%i, dtv%k
end subroutine

end module

program dtio_write_15
use dtio_write_15_mod
implicit none

! Inherits the binding of t.
type, extends(t) :: p2
    integer :: j = 0
end type

! Inherits the overriding binding of t3.
type, extends(t3) :: p4
    integer :: m = 0
end type

type(p2) :: y
type(p4) :: w
class(t), allocatable :: c
character(len=40) :: s

y%i = 2
y%j = 3
w%i = 6
w%k = 7
w%m = 8

ncalls = 0
write(s, '(dt"a",1x,dt"b")') y, w
if (ncalls /= 2) error stop 1
if (s /= '<t:DTa:2> <t3:DTb:6,7>') error stop 2

allocate(c, source=w)
write(s, '(dt)') c
if (s /= '<t3:DT:6,7>') error stop 3
deallocate(c)
allocate(c, source=y)
write(s, '(dt)') c
if (s /= '<t:DT:2>') error stop 4

call local()
print *, 'ok'

contains

! An extended type declared in a procedure.
subroutine local()
    type, extends(t3) :: q
        integer :: r = 0
    end type
    type(q) :: z
    class(t), allocatable :: d
    z%i = 11
    z%k = 12
    write(s, '(dt"l")') z
    if (s /= '<t3:DTl:11,12>') error stop 5
    allocate(d, source=z)
    write(s, '(dt"m")') d
    if (s /= '<t3:DTm:11,12>') error stop 6
end subroutine

end program
