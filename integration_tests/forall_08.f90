module forall_08_mod
implicit none
type :: pair
    integer :: x, y
end type
type :: holder
    integer :: v
end type
type :: row
    integer :: arr(3)
end type
type :: buffers
    integer, allocatable :: src(:), arr(:)
end type
contains
pure integer function total(x)
integer, intent(in) :: x(:)
total = sum(x)
end function

pure function scaled(x) result(r)
integer, intent(in) :: x(:)
integer :: r(size(x))
r = 10 * x
end function

pure integer function total2(x)
integer, intent(in) :: x(:,:)
total2 = sum(x)
end function

subroutine count_matches(c)
character(*), intent(inout) :: c(:)
integer :: i, n
n = size(c)
forall (i = 1:n) c(i) = repeat(achar(48 + count(c == 'abc')), len(c))
end subroutine

subroutine reverse(x)
real, intent(inout) :: x(:)
integer(8) :: k, n
n = size(x, kind=8)
forall (k = 1_8:n) x(k) = x(n + 1 - k)
end subroutine
end module

program forall_08
use forall_08_mod
implicit none
integer :: a(5), m(3,3), m4(4,4), y(3,3), i, j
integer, target :: t(5)
integer, pointer :: p(:)
type(pair) :: arr(4)
type(holder) :: s(4)
character(len=3) :: ch(3)
type(row) :: w(3)
type(buffers) :: b(3)
real :: r(4)
character(len=6) :: str
character(len=9) :: str2

! Every right-hand side is evaluated before any element is assigned.
a(1:4) = [1, 2, 3, 4]
a(5) = 0
forall (i = 1:4) a(i) = a(5-i)
if (any(a /= [4, 3, 2, 1, 0])) error stop 1

do i = 1, 4
    arr(i) = pair(i, 10*i)
end do
forall (i = 1:4) arr(i) = arr(5-i)
if (any(arr%x /= [4, 3, 2, 1])) error stop 2
if (any(arr%y /= [40, 30, 20, 10])) error stop 3

! Array temporaries that read the whole assigned array.
a = [1, 2, 3, 4, 5]
forall (i = 1:5) a(i) = sum(a*2)
if (any(a /= 30)) error stop 4

a = [1, 2, 3, 4, 5]
forall (i = 1:5)
    a(i) = sum(a*2)
end forall
if (any(a /= 30)) error stop 5

a = [1, 2, 3, 4, 5]
forall (i = 1:5) a(i) = total(a+0)
if (any(a /= 15)) error stop 6

a = [1, 2, 3, 4, 5]
forall (i = 1:5) a(i) = sum(scaled(a))
if (any(a /= 150)) error stop 7

a = [1, 2, 3, 4, 5]
forall (i = 1:5) a(i) = size(pack(a, a > 2))
if (any(a /= 3)) error stop 8

m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3, j = 1:3) m(i,j) = sum(m*2)
if (any(m /= 90)) error stop 9

m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3)
    forall (j = 1:3)
        m(i,j) = sum(m*2)
    end forall
end forall
if (any(m /= 90)) error stop 10

do i = 1, 4
    s(i)%v = i
end do
forall (i = 1:4) s(i)%v = sum(s%v * 2)
if (any(s%v /= 20)) error stop 11

! The mask is evaluated for every index before any assignment.
a = [1, 2, 3, 4, 5]
forall (i = 2:5, a(i-1) > 0) a(i) = -a(i-1)
if (any(a /= [1, -1, -2, -3, -4])) error stop 12

a = [1, 2, 3, 4, 5]
forall (i = 1:5, a(6-i) > 2) a(i) = a(6-i)
if (any(a /= [5, 4, 3, 4, 5])) error stop 13

! Strides, empty index spaces and bounds that read the assigned array.
a = [1, 2, 3, 4, 5]
forall (i = 5:1:-2) a(i) = a(6-i)
if (any(a /= [5, 2, 3, 4, 1])) error stop 14
forall (i = 1:0) a(i) = a(6-i)
if (any(a /= [5, 2, 3, 4, 1])) error stop 15
a = [3, 2, 1, 4, 5]
forall (i = 1:a(1)) a(i) = a(1) + i
if (any(a /= [4, 5, 6, 4, 5])) error stop 16

ch = ['abc', 'def', 'ghi']
forall (i = 1:3) ch(i) = ch(4-i)
if (any(ch /= ['ghi', 'def', 'abc'])) error stop 17

! The right-hand side reads the assigned array through a pointer.
t = [1, 2, 3, 4, 5]
p => t
forall (i = 1:5) t(i) = p(6-i)
if (any(t /= [5, 4, 3, 2, 1])) error stop 18

r = [1.0, 2.0, 3.0, 4.0]
call reverse(r)
if (any(r /= [4.0, 3.0, 2.0, 1.0])) error stop 19

! Array-valued variables.
m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3) m(:,i) = total2(m+0)
if (any(m /= 45)) error stop 20

m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3) m(i,:) = m(i,:) + sum(m*1)
if (any(m /= reshape([(i + 45, i = 1, 9)], [3, 3]))) error stop 21

m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3) m(i,:) = m(4-i,:)
if (any(m /= reshape([3, 2, 1, 6, 5, 4, 9, 8, 7], [3, 3]))) error stop 22

m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3) m(i,:) = m(i,:) * 2 + m(1,1)
if (any(m /= reshape([(2*i + 1, i = 1, 9)], [3, 3]))) error stop 23

m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3, m(i,1) > 1) m(i,:) = sum(m*1)
if (any(m /= reshape([1, 45, 45, 4, 45, 45, 7, 45, 45], [3, 3]))) error stop 24

y = 1
forall (j = 1:3) y(1:j,j) = sum(y*1)
if (any(y /= reshape([9, 1, 1, 9, 9, 1, 9, 9, 9], [3, 3]))) error stop 25

do i = 1, 3
    w(i)%arr = [i, 2*i, 3*i]
end do
forall (i = 1:3) w(i)%arr = sum(w%arr(2))
do i = 1, 3
    if (any(w(i)%arr /= 12)) error stop 26
end do
do i = 1, 3
    w(i)%arr = [i, 2*i, 3*i]
end do
forall (i = 1:3) w(i)%arr = w(1)%arr(1) + w(3)%arr(3)
do i = 1, 3
    if (any(w(i)%arr /= 10)) error stop 27
end do
do i = 1, 3
    w(i)%arr = [i, 2*i, 3*i]
end do
forall (i = 1:3) w(i)%arr(:) = w(4-i)%arr(:)
do i = 1, 3
    if (any(w(i)%arr /= [4-i, 2*(4-i), 3*(4-i)])) error stop 28
end do

! Inner index ranges that depend on outer indices.
m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3)
    forall (j = 1:i)
        m(i,j) = sum(m*1)
    end forall
end forall
if (any(m /= reshape([45, 45, 45, 4, 45, 45, 7, 8, 45], [3, 3]))) error stop 29

m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3)
    forall (j = 1:i)
        m(i,j) = m(4-i,4-j)
    end forall
end forall
if (any(m /= reshape([9, 8, 7, 4, 5, 4, 7, 8, 1], [3, 3]))) error stop 30

m4 = 1
forall (i = 1:4, m4(i,i) > 0)
    forall (j = i:4)
        m4(i,j) = sum(m4*1) + i
    end forall
end forall
if (any(m4(1,:) /= 17) .or. m4(2,2) /= 18 .or. m4(4,4) /= 20) error stop 31
if (m4(2,1) /= 1 .or. m4(4,3) /= 1) error stop 32

! Subscripts of the variable are evaluated before any assignment.
a(1:3) = [2, 3, 1]
forall (i = 1:3) a(a(i)) = i
if (any(a(1:3) /= [3, 1, 2])) error stop 33

! A string length that is not a constant.
ch = ['abc', 'def', 'abc']
call count_matches(ch)
if (any(ch /= '222')) error stop 34

! Reading only the part that the same iteration assigns, or other
! components of the variable.
a = [1, 2, 3, 4, 5]
forall (i = 1:5) a(i) = a(i) * 2 + 1
if (any(a /= [3, 5, 7, 9, 11])) error stop 35

m = reshape([(i, i = 1, 9)], [3, 3])
forall (i = 1:3, j = 1:3, m(i,j) > 4) m(i,j) = m(i,j) * a(i)
if (any(m /= reshape([1, 2, 3, 4, 25, 42, 21, 40, 63], [3, 3]))) error stop 36

do i = 1, 4
    arr(i) = pair(i, 10*i)
end do
forall (i = 1:4) arr(i)%x = arr(5-i)%y + arr(i)%x
if (any(arr%x /= [41, 32, 23, 14]) .or. any(arr%y /= [10, 20, 30, 40])) error stop 37

! Allocatable components take the shape of the value.
do i = 1, 3
    allocate(b(i)%src(i))
    b(i)%src = i
end do
forall (i = 1:3) b(i)%arr = b(i)%src
do i = 1, 3
    if (size(b(i)%arr) /= i .or. any(b(i)%arr /= i)) error stop 38
    deallocate(b(i)%arr)
end do
forall (i = 1:3) b(i)%arr = b(4-i)%src
do i = 1, 3
    if (size(b(i)%arr) /= 4-i .or. any(b(i)%arr /= 4-i)) error stop 39
    deallocate(b(i)%arr)
end do
forall (i = 1:3) b(i)%arr = ramp(i)
do i = 1, 3
    if (size(b(i)%arr) /= i .or. any(b(i)%arr /= 10*i)) error stop 40
end do

! Substring bounds of the variable that read it.
str = '231xyz'
forall (i = 1:3) str(ichar(str(i:i))-48:ichar(str(i:i))-48) = achar(96+i)
if (str /= 'cabxyz') error stop 41
str = '135xyz'
forall (i = 1:3) str(ichar(str(i:i))-48:ichar(str(i:i))-47) = achar(64+i)//achar(96+i)
if (str /= 'AaBbCc') error stop 42
str2 = '147abcdef'
forall (i = 1:3) str2(ichar(str2(i:i))-48:ichar(str2(i:i))-46) = achar(64+i)
if (str2 /= 'A  B  C  ') error stop 43
str2 = '147abcdef'
forall (i = 1:3) str2(ichar(str2(i:i))-48:ichar(str2(i:i))-47) = 'PQRS'
if (str2 /= 'PQ7PQcPQf') error stop 44
print *, "ok"

contains

pure function ramp(n) result(res)
integer, intent(in) :: n
integer :: res(n)
res = 10 * n
end function
end program
