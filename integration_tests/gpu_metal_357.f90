program gpu_metal_357
! A kernel gives an allocatable component of struct array elements a size,
! in a `do concurrent` that writes some of the elements only. With
! --realloc-lhs-arrays the host allocates, or allocates again, exactly the
! elements the loop writes, and never sizes one from a value the loop
! changes.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(5)
integer :: m(2), p(3), s(3)
type(tt) :: t(5), u(3), w(4), x(3), y(3), q(3), z(4)
integer :: i

c = [1.0, 2.0, 3.0, 4.0, 5.0]

! The extent reads m(i), which has no element past the loop's range.
m = [2, 3]
do concurrent (i = 1:2)
    t(i) = f(c(1:m(i)))
end do
if (size(t(1)%v) /= 2) error stop 1
if (size(t(2)%v) /= 3) error stop 2
if (any(t(2)%v /= c(1:3))) error stop 3
if (allocated(t(3)%v)) error stop 4
if (allocated(t(5)%v)) error stop 5

! The loop starts past the first element.
p = [0, 2, 3]
do concurrent (i = 2:3)
    u(i) = f(c(1:p(i)))
end do
if (allocated(u(1)%v)) error stop 6
if (size(u(2)%v) /= 2) error stop 7
if (size(u(3)%v) /= 3) error stop 8
if (any(u(3)%v /= c(1:3))) error stop 9

! The element is picked by an expression of the loop index.
do concurrent (i = 1:2)
    w(i+1) = h(c, i)
end do
if (allocated(w(1)%v)) error stop 10
if (size(w(2)%v) /= 1) error stop 11
if (size(w(3)%v) /= 2) error stop 12
if (any(w(3)%v /= c(1:2))) error stop 13
if (allocated(w(4)%v)) error stop 14

! The host cannot compute the extent, and the caller allocated every
! element the loop writes; x(3) is not written and stays unallocated.
do i = 1, 2
    allocate(x(i)%v(g(i)))
end do
do concurrent (i = 1:2)
    x(i) = f(c(1:g(i)))
end do
if (size(x(1)%v) /= 2) error stop 15
if (size(x(2)%v) /= 3) error stop 16
if (any(x(2)%v /= c(1:3))) error stop 17
if (allocated(x(3)%v)) error stop 18

! The extent reads s(i), which the loop writes first, so the value before
! the loop is not the size; the caller allocated each element to the size
! the loop gives it.
s = 1
do i = 1, 3
    allocate(y(i)%v(i + 1))
end do
do concurrent (i = 1:3)
    s(i) = i + 1
    y(i) = f(c(1:s(i)))
end do
do i = 1, 3
    if (size(y(i)%v) /= i + 1) error stop 19
    if (any(y(i)%v /= c(1:i+1))) error stop 20
end do

! Components allocated with another size are allocated again, with a size
! known at run time.
do i = 1, 3
    allocate(q(i)%v(1))
end do
do concurrent (i = 1:3)
    q(i) = f(c(1:i+1))
end do
do i = 1, 3
    if (size(q(i)%v) /= i + 1) error stop 21
    if (any(q(i)%v /= c(1:i+1))) error stop 22
end do

! And with a size known when compiling; z(3:4) are not written.
allocate(z(1)%v(1))
do concurrent (i = 1:2)
    z(i) = f(c(1:3))
end do
if (size(z(1)%v) /= 3) error stop 23
if (size(z(2)%v) /= 3) error stop 24
if (any(z(1)%v /= c(1:3))) error stop 25
if (allocated(z(3)%v)) error stop 26
if (allocated(z(4)%v)) error stop 27

print *, "ok"

contains

pure integer function g(j)
    integer, intent(in) :: j
    g = j + 1
end function

pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function

pure function h(a, n) result(r)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    type(tt) :: r
    allocate(r%v(n))
    r%v = a(1:n)
end function

end program
