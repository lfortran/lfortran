! Defined output of an implied-do loop over items of different derived types:
! each item is passed to the defined output procedure of its own type, and a
! derived-type item without one is written as its components.
module dtio_write_11_mod
implicit none
character(200) :: calls = ''

type :: t
    integer :: a = 1
contains
    procedure :: wt
    generic :: write(formatted) => wt
end type

type :: u
    real :: r = 2.5
    integer :: k = 9
contains
    procedure :: wu
    generic :: write(formatted) => wu
end type

contains

subroutine wt(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    character(20) :: s
    write(s, '("T",i0)') dtv%a
    calls = trim(calls) // ' ' // trim(s)
    write(unit, '("T",i0)', iostat=iostat) dtv%a
end subroutine

subroutine wu(dtv, unit, iotype, v_list, iostat, iomsg)
    class(u), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    character(20) :: s
    write(s, '("U",i0)') dtv%k
    calls = trim(calls) // ' ' // trim(s)
    write(unit, '("U",i0)', iostat=iostat) dtv%k
end subroutine

end module

program dtio_write_11
use dtio_write_11_mod
implicit none
type :: plain
    integer :: k = 7
    real :: r = 1.5
end type
type(t) :: a(2)
type(u) :: c(2)
type(plain) :: b(2)
integer :: i, n, out
character(80) :: line

n = 2
a(2)%a = 4
c(2)%k = 8

print '(*(dt,1x,dt,1x))', (a(i), c(i), i=1,2)
call check_calls('T1 U9 T4 U8', 1)
print '(dt,1x,dt)', (a(i), c(i), i=1,n)
call check_calls('T1 U9 T4 U8', 2)
print *, (a(i), c(i), i=1,2)
call check_calls('T1 U9 T4 U8', 3)
print '(dt,1x,i0,1x,f3.1)', (a(i), b(i), i=1,n)
call check_calls('T1 T4', 4)
print '(*(dt,1x,i0,1x,f3.1,1x))', (a(i), b(i), i=1,2)
call check_calls('T1 T4', 5)

open(newunit=out, status='scratch', action='readwrite', form='formatted')
write(out, '(*(dt,1x,dt,1x))') (a(i), c(i), i=1,2)
write(out, '(dt,1x,dt)') (a(i), c(i), i=1,n)
write(out, '(dt,1x,i0,1x,f3.1)') (a(i), b(i), i=1,n)
write(out, '(*(dt,1x,i0,1x,f3.1,1x))') (a(i), b(i), i=1,2)
call check_calls('T1 U9 T4 U8 T1 U9 T4 U8 T1 T4 T1 T4', 6)
rewind(out)
call check_line(out, 'T1 U9 T4 U8', 7)
call check_line(out, 'T1 U9', 8)
call check_line(out, 'T4 U8', 9)
call check_line(out, 'T1 7 1.5', 10)
call check_line(out, 'T4 7 1.5', 11)
call check_line(out, 'T1 7 1.5 T4 7 1.5', 12)
close(out)
print *, 'ok'

contains

subroutine check_calls(expected, code)
    character(*), intent(in) :: expected
    integer, intent(in) :: code
    if (trim(adjustl(calls)) /= expected) then
        print *, 'calls "', trim(adjustl(calls)), '" /= "', expected, '"'
        error stop code
    end if
    calls = ''
end subroutine

subroutine check_line(unit, expected, code)
    integer, intent(in) :: unit, code
    character(*), intent(in) :: expected
    character(80) :: got
    read(unit, '(a)') got
    if (trim(got) /= expected) then
        print *, 'line "', trim(got), '" /= "', expected, '"'
        error stop code
    end if
end subroutine

end program
