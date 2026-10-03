program gpu_metal_367
! A function that gives the component of its result the same size on every
! branch: a constant size, and a size from its dummy argument. With
! --realloc-lhs-arrays the host allocates the written components with that
! size before the launch; the element the loop does not write keeps its
! size and data.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6)
type(tt) :: t(3), u(3)
integer :: i
c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
allocate(t(3)%v(4), u(3)%v(4))
t(3)%v = -1.0
u(3)%v = -2.0
do concurrent (i = 1:2)
    t(i) = h(c, i)
end do
print *, size(t(1)%v), size(t(2)%v), t(1)%v, t(2)%v
if (size(t(1)%v) /= 3 .or. size(t(2)%v) /= 3) error stop 1
if (any(t(1)%v /= c(4:6))) error stop 2
if (any(t(2)%v /= c(1:3))) error stop 3
if (size(t(3)%v) /= 4 .or. any(t(3)%v /= -1.0)) error stop 4
do concurrent (i = 1:2)
    u(i) = g(c, i)
end do
print *, size(u(1)%v), size(u(2)%v), u(1)%v, u(2)%v
if (size(u(1)%v) /= 2 .or. size(u(2)%v) /= 3) error stop 5
if (any(u(1)%v /= c(2:3))) error stop 6
if (any(u(2)%v /= [1.0, 2.0, 3.0])) error stop 7
if (size(u(3)%v) /= 4 .or. any(u(3)%v /= -2.0)) error stop 8
print *, "ok"
contains
pure function h(a, k) result(r)
    real, intent(in) :: a(:)
    integer, intent(in) :: k
    type(tt) :: r
    if (k > 1) then
        allocate(r%v(3))
        r%v = a(1:3)
    else
        allocate(r%v(3))
        r%v = a(4:6)
    end if
end function

pure function g(a, k) result(r)
    real, intent(in) :: a(:)
    integer, intent(in) :: k
    type(tt) :: r
    integer :: j
    if (k > 1) then
        allocate(r%v(k + 1))
        do j = 1, k + 1
            r%v(j) = a(j)
        end do
    else
        allocate(r%v(k + 1))
        do j = 1, k + 1
            r%v(j) = a(j + 1)
        end do
    end if
end function
end program
