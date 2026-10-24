! The parent statement continues at the position where the output of a
! defined output procedure (child) ended, not at the end of that output:
! after TL in the child, the parent's next field blank-fills the positions
! skipped since then, and the child's output after that position is kept
! otherwise (#14961). A trailing TL or T that no field follows does not
! move the child's end back before its last field. Records are compared
! exactly, including trailing blanks. Parent TR or X over positions that the
! parent wrote before the child is not covered: libgfortran 14 keeps those
! characters, libgfortran 16 blank-fills them.
module dtio_write_14_m
implicit none
type :: t
    integer :: k = 0
contains
    procedure :: out
    generic :: write(formatted) => out
end type
type :: w
    type(t) :: inner
contains
    procedure :: wout
    generic :: write(formatted) => wout
end type
contains
subroutine out(dtv, unit, iotype, v_list, iostat, iomsg)
    class(t), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    iostat = 0
    select case (dtv%k)
    case (0)
        write(unit, '(a,tl2,a)', iostat=iostat, iomsg=iomsg) 'abc', 'X'
    case (1)
        write(unit, '(b2)', iostat=iostat, iomsg=iomsg) 7
    case (2)
        write(unit, '(b5)', iostat=iostat, iomsg=iomsg) 3
    case (3)
        write(unit, '(tr2,a)', iostat=iostat, iomsg=iomsg) 'X'
    case (4)
        write(unit, '(a,tl5,a)', iostat=iostat, iomsg=iomsg) 'abcdef', 'X'
    case (5)
        write(unit, '(a,tr3)', iostat=iostat, iomsg=iomsg) 'ab'
    case (6)
        ! No output.
    case (7)
        write(unit, '(a,tl2,a,/,a,tl2,a)', iostat=iostat, iomsg=iomsg) &
            'abc', 'X', 'def', 'Y'
    case (8)
        write(unit, '(a,tl2)', iostat=iostat, iomsg=iomsg) 'abc'
    case (9)
        write(unit, '(a,t2)', iostat=iostat, iomsg=iomsg) 'abc'
    case (10)
        write(unit, '(a,tl2)', iostat=iostat, iomsg=iomsg) 'abc'
        if (iostat == 0) write(unit, '(a)', iostat=iostat, iomsg=iomsg) 'Y'
    case (11)
        write(unit, '(a,tl2)', advance='no', iostat=iostat, iomsg=iomsg) 'abc'
        if (iostat == 0) write(unit, '(a)', iostat=iostat, iomsg=iomsg) 'X'
    case (12)
        write(unit, '(a,tl2,a,tl1)', iostat=iostat, iomsg=iomsg) 'abc', 'X'
    end select
end subroutine
subroutine wout(dtv, unit, iotype, v_list, iostat, iomsg)
    class(w), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    write(unit, '(dt,tr1,a)', iostat=iostat, iomsg=iomsg) dtv%inner, 'N'
end subroutine
end module

program dtio_write_14
use dtio_write_14_m
implicit none
type(t) :: x0, xb2, xb5, xtr, x5, xtr3, xnone, xrec
type(t) :: xtl, xt, xtl2, xadv, xtl3
type(w) :: y
character(len=40) :: s
integer :: u
x0%k = 0
xb2%k = 1
xb5%k = 2
xtr%k = 3
x5%k = 4
xtr3%k = 5
xnone%k = 6
xrec%k = 7
xtl%k = 8
xt%k = 9
xtl2%k = 10
xadv%k = 11
xtl3%k = 12
y%inner%k = 0

open(newunit=u, status='scratch', action='readwrite', form='formatted')
write(u, '(dt,tr1,a)') x0, 'Z'
write(u, '(dt,dt)') x0, x0
write(u, '(dt,t2,a)') x0, 'Z'
write(u, '(dt)') x0
write(u, '(dt,tr3)') x0
write(u, '(dt,tl1,tr2,a)') x0, 'Z'
write(u, '(dt,tr1,"lit")') x0
write(u, '(a,tl4,dt,a)') 'abcdefgh', x0, 'Z'
write(u, '(dt,tr1,a,tr1,a)') x5, 'Z', 'W'
write(u, '(dt,tr1,a,tl3,a)') x5, 'Z', 'W'
write(u, '(dt,1x,a)') x5, 'Z'
write(u, '(2(dt,tr1,a))') x5, 'Z', x5, 'W'
write(u, '(dt,a)') xtr3, 'Z'
write(u, '(a,tl3,dt,a)') 'abcdef', xnone, 'Z'
write(u, '(dt,a)') xb2, 'Z'
write(u, '(a,tl6,dt)') 'abcdef', xb2
write(u, '(a,tl6,dt)') 'abcdef', xb5
write(u, '(a,tl6,dt)') 'abcdef', xtr
write(u, '(dt,tr1,a)') y, 'Z'
write(u, '(a,tl7,dt)') 'abcdefg', y
write(u, '(dt,tr1,a)') xrec, 'Z'
write(u, '(dt,a)') xtl, 'Z'
write(u, '(dt,a)') xt, 'Z'
write(u, '(dt,a)') xtl2, 'Z'
write(u, '(dt,tr1,a)') xadv, 'Z'
write(u, '(dt,a)') xtl3, 'Z'
write(u, *) xtr3, 'Z'
write(u, *) xtr3
rewind(u)
call check(u, 'aX Z', 1)
call check(u, 'aXaXc', 2)
call check(u, 'aZc', 3)
call check(u, 'aXc', 4)
call check(u, 'aXc', 5)
call check(u, 'aX Z', 6)
call check(u, 'aX lit', 7)
call check(u, 'abcdaXZh', 8)
call check(u, 'aX Z W', 9)
call check(u, 'aW Zef', 10)
call check(u, 'aX Zef', 11)
call check(u, 'aX ZaX Wef', 12)
call check(u, 'ab   Z', 13)
call check(u, 'abcZef', 14)
call check(u, '**Z', 15)
call check(u, '**cdef', 16)
call check(u, '   11f', 17)
call check(u, '  Xdef', 18)
call check(u, 'aX N Z', 19)
call check(u, 'aX Nefg', 20)
call check(u, 'aXc', 21)
call check(u, 'dY Z', 22)
call check(u, 'abcZ', 23)
call check(u, 'abcZ', 24)
call check(u, 'abcYZ', 25)
call check(u, 'abcX Z', 26)
call check(u, 'aXZ', 27)
! List-directed: gfortran writes a leading blank, LFortran does not.
call check(u, 'ab    Z', 28, .true.)
call check(u, 'ab   ', 29, .true.)
close(u)

! Internal-file parents.
write(s, '(dt,tr2,a)') x0, 'Z'
if (s /= 'aX  Z') error stop 31
write(s, '(a,tl4,dt,a)') 'abcdefgh', x0, 'Z'
if (s /= 'abcdaXZh') error stop 32
write(s, '(dt,a)') xb2, 'Z'
if (s /= '**Z') error stop 33
write(s, '(a,tl6,dt)') 'abcdef', xb5
if (s /= '   11f') error stop 34
write(s, '(dt,tr1,a)') y, 'Z'
if (s /= 'aX N Z') error stop 35
write(s, '(dt,a)') xtl, 'Z'
if (s /= 'abcZ') error stop 36
write(s, '(dt,a)') xtl2, 'Z'
if (s /= 'abcYZ') error stop 37
print '(a)', 'ok'

contains

! Reads the next record and compares it with `expected`, including its
! length. With `lead`, one leading blank of the record is ignored.
subroutine check(unit, expected, n, lead)
    integer, intent(in) :: unit, n
    character(*), intent(in) :: expected
    logical, intent(in), optional :: lead
    character(len=40) :: line
    integer :: length, first, ios
    read(unit, '(a)', advance='no', size=length, iostat=ios) line
    if (.not. is_iostat_eor(ios)) error stop 100 + n
    first = 1
    if (present(lead)) then
        if (lead .and. length > 0) then
            if (line(1:1) == ' ') first = 2
        end if
    end if
    if (length - first + 1 /= len(expected)) then
        print '(5a)', '"', line(first:length), '" /= "', expected, '"'
        error stop n
    end if
    if (line(first:length) /= expected) then
        print '(5a)', '"', line(first:length), '" /= "', expected, '"'
        error stop n
    end if
end subroutine

end program
