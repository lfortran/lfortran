! Defined formatted I/O with -fdefault-integer-8: the unit, v_list and iostat
! arguments of the user procedure are integer(8)
module ilp64_dtio_01_m
implicit none
type :: t
   integer :: i = 0
end type
interface write(formatted)
   module procedure wf
end interface
interface read(formatted)
   module procedure rf
end interface
contains
subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
   class(t), intent(in) :: dtv
   integer, intent(in) :: unit, v_list(:)
   character(len=*), intent(in) :: iotype
   integer, intent(out) :: iostat
   character(len=*), intent(inout) :: iomsg
   if (size(v_list) /= 0) error stop
   write(unit, '(a,i0)', iostat=iostat) "T", dtv%i
end subroutine
subroutine rf(dtv, unit, iotype, v_list, iostat, iomsg)
   class(t), intent(inout) :: dtv
   integer, intent(in) :: unit, v_list(:)
   character(len=*), intent(in) :: iotype
   integer, intent(out) :: iostat
   character(len=*), intent(inout) :: iomsg
   if (size(v_list) /= 0) error stop
   read(unit, '(1x,i1)', iostat=iostat) dtv%i
end subroutine
end module

program ilp64_dtio_01
use ilp64_dtio_01_m
implicit none
type(t) :: x
integer :: u, ios
integer(4) :: u4, ios4
character(10) :: line

x%i = 5
write(*, *) x

u = 10
open(u, file='ilp64_dtio_01.txt', status='replace')
ios = 99
write(u, '(dt)', iostat=ios) x
if (ios /= 0) error stop
close(u)

x%i = 42
open(u, file='ilp64_dtio_01.txt', status='old', position='append')
u4 = int(u, 4)
ios4 = 99
write(u4, '(dt)', iostat=ios4) x
if (ios4 /= 0) error stop
close(u)

open(u, file='ilp64_dtio_01.txt', status='old')
read(u, '(a)') line
if (line /= 'T5') error stop
read(u, '(a)') line
if (line /= 'T42') error stop
rewind(u)
x%i = 0
ios4 = 99
read(u, '(dt)', iostat=ios4) x
if (ios4 /= 0) error stop
if (x%i /= 5) error stop
close(u, status='delete')
print *, "ok"
end program
