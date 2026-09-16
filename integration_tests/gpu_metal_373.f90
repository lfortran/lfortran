program gpu_metal_373
! An allocatable component written in more than one place in an offloaded
! loop: in both branches of an `if`, in two cases of a `select case`, twice
! in one iteration, and in branches that give it different sizes. With
! --realloc-lhs-arrays the host allocates each written component before the
! launch; the element the loop does not write keeps its size and data.
implicit none
type tt
    real, allocatable :: v(:)
end type
type(tt) :: a(5), b(5), c(5), d(5)
real :: x(6)
integer :: i, n
x = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
n = 3
allocate(a(5)%v(4), b(5)%v(4), c(5)%v(4), d(5)%v(4))
a(5)%v = -1.0
b(5)%v = -2.0
c(5)%v = -3.0
d(5)%v = -4.0

do concurrent (i = 1:4)
    if (mod(i, 2) == 0) then
        a(i) = k(1.0)
    else
        a(i) = k(2.0)
    end if
end do
print *, size(a(1)%v), size(a(2)%v), a(1)%v(3), a(2)%v(3)
do i = 1, 4
    if (size(a(i)%v) /= 3) error stop 1
    if (mod(i, 2) == 0 .and. any(a(i)%v /= 1.0)) error stop 2
    if (mod(i, 2) == 1 .and. any(a(i)%v /= 2.0)) error stop 3
end do
if (size(a(5)%v) /= 4 .or. any(a(5)%v /= -1.0)) error stop 4

do concurrent (i = 1:4)
    select case (mod(i, 2))
    case (0)
        b(i) = k(1.0)
    case default
        b(i) = k(2.0)
    end select
end do
print *, size(b(1)%v), size(b(2)%v), b(1)%v(3), b(2)%v(3)
do i = 1, 4
    if (size(b(i)%v) /= 3) error stop 5
    if (mod(i, 2) == 0 .and. any(b(i)%v /= 1.0)) error stop 6
    if (mod(i, 2) == 1 .and. any(b(i)%v /= 2.0)) error stop 7
end do
if (size(b(5)%v) /= 4 .or. any(b(5)%v /= -2.0)) error stop 8

do concurrent (i = 1:4)
    c(i) = k(1.0)
    c(i) = k(real(i))
end do
print *, size(c(4)%v), c(4)%v
do i = 1, 4
    if (size(c(i)%v) /= 3 .or. any(c(i)%v /= real(i))) error stop 9
end do
if (size(c(5)%v) /= 4 .or. any(c(5)%v /= -3.0)) error stop 10

do concurrent (i = 1:4)
    if (mod(i, 2) == 0) then
        d(i) = g(x(1:n - 1))
    else
        d(i) = g(x(1:n) * 2.0)
    end if
end do
print *, size(d(1)%v), size(d(2)%v), d(1)%v, d(2)%v
do i = 1, 4
    if (mod(i, 2) == 0) then
        if (size(d(i)%v) /= 2 .or. any(d(i)%v /= x(1:2))) error stop 11
    else
        if (size(d(i)%v) /= 3 .or. any(d(i)%v /= 2.0 * x(1:3))) error stop 12
    end if
end do
if (size(d(5)%v) /= 4 .or. any(d(5)%v /= -4.0)) error stop 13
print *, "ok"
contains
pure function k(s) result(r)
    real, intent(in) :: s
    type(tt) :: r
    allocate(r%v(3))
    r%v = s
end function

pure function g(s) result(r)
    real, intent(in) :: s(:)
    type(tt) :: r
    allocate(r%v(size(s)))
    r%v = s
end function
end program
