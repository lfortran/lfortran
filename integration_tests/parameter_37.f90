program parameter_37
! A substring of an element of a character named constant array
implicit none
type :: t
    character(len=3) :: names(2)
end type
character(len=3), parameter :: pc(-1:1) = ['abc', 'def', 'ghi']
character(len=3), parameter :: pd(3) = ['abc', 'def', 'ghi']
character(len=4), parameter :: p2(2, 2) = &
    reshape(['abcd', 'efgh', 'ijkl', 'mnop'], [2, 2])
character(len=5), parameter :: pp(2) = [character(len=5) :: 'ab', 'cdefg']
character(kind=4, len=3), parameter :: p4(2) = [4_'abc', 4_'def']
type(t), parameter :: s = t(['abc', 'def'])
character(len=3) :: vc(3) = ['abc', 'def', 'ghi']
character(len=2), parameter :: x = pd(2)(2:3)
character(len=*), parameter :: y = pc(0)(:2)
character(len=*), parameter :: z = p2(2, 1)(2:)
character(len=*), parameter :: e = pd(1)(3:2)
integer, parameter :: n = len(pd(3)(2:3))
integer, parameter :: code = iachar(pd(2)(1:1))
logical, parameter :: same = pd(2)(2:3) == 'ef'
character(len=len(pd(2)(2:3))) :: w
character(len=40) :: line
integer :: i, lo, hi

if (pc(0)(2:3) /= 'ef') error stop
if (pd(2)(2:3) /= 'ef') error stop
if (vc(2)(2:3) /= 'ef') error stop
if (x /= 'ef') error stop
if (pd(2)(1:1) /= 'd') error stop
if (pd(2)(3:3) /= 'f') error stop
if (pd(3)(:) /= 'ghi') error stop
if (len(pd(2)(2:3)) /= 2) error stop
if (len(pd(2)(1:1)) /= 1) error stop
if (len(pd(1)(3:2)) /= 0) error stop
if (len(y) /= 2 .or. y /= 'de') error stop
if (len(z) /= 3 .or. z /= 'fgh') error stop
if (len(e) /= 0) error stop
if (n /= 2) error stop
if (code /= iachar('d')) error stop
if (.not. same) error stop
if (len(w) /= 2) error stop
if (p2(1, 2)(2:3) /= 'jk') error stop
if (p2(2, 2)(4:4) /= 'p') error stop
if (iachar(pc(-1)(3:3)) /= iachar('c')) error stop
if (pd(1)(1:2) // pd(3)(2:3) /= 'abhi') error stop
if (index(pd(2)(2:3), 'f') /= 2) error stop

! The element is blank padded to the length of the array
if (pp(1)(2:4) /= 'b  ') error stop
if (len(pp(1)(2:4)) /= 3) error stop
if (len_trim(pp(1)(2:4)) /= 1) error stop
if (pp(2)(4:5) /= 'fg') error stop

if (p4(2)(2:3) /= 4_'ef') error stop
if (len(p4(2)(2:3)) /= 2) error stop
if (p4(1)(3:3) /= 4_'c') error stop

if (s%names(2)(2:3) /= 'ef') error stop
if (len(s%names(1)(1:1)) /= 1) error stop

write(line, '(5("[",a,"]"))') pc(0)(2:3), pd(2)(2:3), vc(2)(2:3), x, pd(2)(1:1)
if (line /= '[ef][ef][ef][ef][d]') error stop

call check(pd(2)(2:3), 'ef')
call check(pc(1)(:1), 'g')
call check(pd(1)(3:2), '')

! The element or the bounds are known only at run time
lo = 2
hi = 3
do i = 1, 3
    if (pd(i)(2:3) /= vc(i)(2:3)) error stop
    if (pc(i - 2)(lo:hi) /= vc(i)(2:3)) error stop
    if (pd(i)(lo:) /= vc(i)(2:3)) error stop
    if (len(pd(i)(lo:hi)) /= 2) error stop
end do
if (pd(2)(lo:hi) /= 'ef') error stop
if (pd(2)(lo:3) /= 'ef') error stop
if (pd(2)(:hi) /= 'def') error stop
if (pd(2)(hi:lo) /= '') error stop

print *, pc(0)(2:3), pd(2)(2:3), vc(2)(2:3), x, pd(2)(1:1)

contains

    subroutine check(str, expected)
        character(len=*), intent(in) :: str, expected
        if (len(str) /= len(expected)) error stop
        if (str /= expected) error stop
    end subroutine

end program
