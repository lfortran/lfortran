! Defined output (DT edit descriptor) of arrays and implied-do loops of
! derived-type items: each element is one effective item, in array element
! order, passed to the defined output procedure.
module dtio_write_02_mod
implicit none
integer :: ncalls = 0

type :: t
    integer :: v = 0
contains
    procedure :: wf
    generic :: write(formatted) => wf
end type

type :: holder
    type(t) :: items(3)
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
    write(unit, '("<",a,":",i0,">")', iostat=iostat) iotype, dtv%v
end subroutine

! An assumed-shape dummy, associated with a non-contiguous section.
subroutine write_all(unit, x)
    integer, intent(in) :: unit
    type(t), intent(in) :: x(:)
    write(unit, '(*(dt,1x))') x
end subroutine

function mk(n) result(r)
    integer, intent(in) :: n
    type(t) :: r(n)
    integer :: i
    do i = 1, n
        r(i)%v = 10 * i
    end do
end function

end module

program dtio_write_02
use dtio_write_02_mod
implicit none
type(t) :: a(2), b(5), c(2, 2)
type(t), allocatable :: d(:)
type(holder) :: h
integer :: i, j, n, out, line_no

do i = 1, 2
    a(i)%v = i
end do
do i = 1, 5
    b(i)%v = 10 + i
end do
do j = 1, 2
    do i = 1, 2
        c(i, j)%v = 10 * i + j
    end do
end do
allocate(d(3))
do i = 1, 3
    d(i)%v = 20 + i
    h%items(i)%v = 30 + i
end do
n = 3
open(newunit=out, status='scratch', action='readwrite', form='formatted')

write(out, '(2(dt,1x))') a
write(out, '(3(dt,1x))') d
write(out, '(dt,1x,i0)') a(1:1), 5
write(out, '(i0,1x,dt,1x,dt)') 5, a
write(out, '(3(dt,1x))') b(1:5:2)
write(out, '(3(dt,1x))') (b(i), i=1,n)
write(out, '(2(dt,1x))') (a(i), i=1,2)
write(out, '(3(i0,dt,1x))') (i, b(i), i=1,3)
write(out, '(4(dt,1x))') ((c(i, j), i=1,2), j=n-1,1,-1)
write(out, '(*(dt:,","))') b
write(out, '(4(dt,1x))') c
write(out, '(3(dt,1x))') h%items
write(out, '(2(dt,1x))') mk(2)
call write_all(out, b(5:1:-2))
ncalls = 0
print '(2(dt,1x))', a
print '(*(dt:,","))', (b(i), i=2,n)
if (ncalls /= 4) error stop 1

rewind(out)
line_no = 0
call check('<DT:1> <DT:2>')
call check('<DT:21> <DT:22> <DT:23>')
call check('<DT:1> 5')
call check('5 <DT:1> <DT:2>')
call check('<DT:11> <DT:13> <DT:15>')
call check('<DT:11> <DT:12> <DT:13>')
call check('<DT:1> <DT:2>')
call check('1<DT:11> 2<DT:12> 3<DT:13>')
call check('<DT:12> <DT:22> <DT:11> <DT:21>')
call check('<DT:11>,<DT:12>,<DT:13>,<DT:14>,<DT:15>')
call check('<DT:11> <DT:21> <DT:12> <DT:22>')
call check('<DT:31> <DT:32> <DT:33>')
call check('<DT:10> <DT:20>')
call check('<DT:15> <DT:13> <DT:11>')
close(out)
print *, 'ok'

contains

subroutine check(expected)
    character(*), intent(in) :: expected
    character(len=64) :: line
    line_no = line_no + 1
    read(out, '(a)') line
    if (trim(line) /= expected) then
        print *, 'line ', line_no, ': "', trim(line), '" /= "', expected, '"'
        error stop 10
    end if
end subroutine

end program
