! Defined output (DT edit descriptor) through the runtime formatter, with
! explicit formats: DT items mixed with other items and edit descriptors,
! format reversion, positioning across the child output, advance=, a unit and
! an item that are function references (evaluated once), nested child output,
! and a DT item in an ASSOCIATE construct and in an internal procedure.
module dtio_write_01_mod
implicit none
integer :: ncalls = 0, last_unit = -1, nunit = 0, nmk = 0, out = -1
character(len=16) :: last_iotype = ''

type :: t
    integer :: v = 0
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type

type :: pair
    type(t) :: a, b
contains
    procedure :: wp
    generic :: write(formatted) => wp
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
    write(unit, '("<",a,":",i0,">")', iostat=iostat) iotype, dtv%v
end subroutine

! Child output that itself writes derived-type items with defined output.
subroutine wp(dtv, unit, iotype, v_list, iostat, iomsg)
    class(pair), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '(dt,"+",dt)', iostat=iostat) dtv%a, dtv%b
end subroutine

integer function unit_fn()
    nunit = nunit + 1
    unit_fn = out
end function

function mk(i) result(r)
    integer, intent(in) :: i
    type(t) :: r
    nmk = nmk + 1
    r%v = i
end function

subroutine in_associate(y)
    type(t), intent(in) :: y
    associate (w => y%v)
        write(out, '(dt,1x,i0)') y, w
        print '(dt)', y
    end associate
end subroutine

end module

program dtio_write_01
use dtio_write_01_mod
implicit none
type(t) :: y, z
type(pair) :: p
character(len=:), allocatable :: f
integer :: n

y%v = 1
z%v = 2
p%a%v = 3
p%b%v = 4
open(newunit=out, status='scratch', action='readwrite', form='formatted')

write(out, '(dt,1x,dt)') y, z
write(out, '(a,dt)') 'x', y
write(out, '(i0,1x,dt)') 3, y
write(out, '(dt,1x,i0)') y, 4
f = '(dt,1x,dt)'
write(out, f) y, z
write(out, '(dt)') y, z
write(out, '(sp,i3,1x,dt)') 5, y
write(out, '(1p,e12.4,1x,dt)') 1.5, y
write(out, '(dt,t10,a)') y, 'X'
write(out, '(dt,tr3,i0)') y, 5
write(out, '(dt,tl3,a)') y, 'ZZ'
write(out, '(dt,1x,dt)', advance='yes') y, z
write(out, '(dt)', advance='no') y
write(out, '(a)') ' tail'
write(unit_fn(), '(dt,1x,dt)') y, z
if (nunit /= 1) error stop 1
write(out, '(dt)') mk(7)
if (nmk /= 1) error stop 2
write(out, '("[",dt,"]")') p
write(out, "(dt'abc')") y
if (last_iotype /= 'DTabc') error stop 3
if (last_unit /= out) error stop 4
call in_associate(y)
call internal()

ncalls = 0
print '(dt,1x,dt)', y, z
if (ncalls /= 2) error stop 5
if (last_unit /= 6) error stop 6
if (last_iotype /= 'DT') error stop 7

rewind(out)
n = 0
call check('<DT:1> <DT:2>')
call check('x<DT:1>')
call check('3 <DT:1>')
call check('<DT:1> 4')
call check('<DT:1> <DT:2>')
call check('<DT:1>')
call check('<DT:2>')
call check(' +5 <DT:1>')
call check('  1.5000E+00 <DT:1>')
call check('<DT:1>   X')
call check('<DT:1>   5')
call check('<DTZZ>')
call check('<DT:1> <DT:2>')
call check('<DT:1> tail')
call check('<DT:1> <DT:2>')
call check('<DT:7>')
call check('[<DT:3>+<DT:4>]')
call check('<DTabc:1>')
call check('<DT:1> 1')
call check('<DT:1>')
close(out)
print *, 'ok'

contains

subroutine internal()
    write(out, '(dt)') y
    print '(dt)', z
end subroutine

subroutine check(expected)
    character(*), intent(in) :: expected
    character(len=64) :: line
    n = n + 1
    read(out, '(a)') line
    if (trim(line) /= expected) then
        print *, 'line ', n, ': "', trim(line), '" /= "', expected, '"'
        error stop 10
    end if
end subroutine

end program
