program gpu_metal_374
! An allocatable component written inside an inner `do` of an offloaded
! loop, alone and together with a write at the top of the body. The host
! cannot tell which elements such a write picks, but every write gives the
! component the same constant size, so with --realloc-lhs-arrays the host
! allocates the components that are not allocated with that size before the
! launch. An allocated element the loop does not write keeps its size and
! data.
implicit none
type tt
    real, allocatable :: v(:)
end type
type(tt) :: t(5), u(5)
integer :: i, j, m
allocate(t(5)%v(4), u(5)%v(4))
t(5)%v = -1.0
u(5)%v = -2.0

do concurrent (i = 1:4)
    do j = 1, 1
        t(i) = k(real(i + j))
    end do
end do
print *, size(t(4)%v), t(4)%v
do i = 1, 4
    if (size(t(i)%v) /= 3 .or. any(t(i)%v /= real(i + 1))) error stop 1
end do
if (size(t(5)%v) /= 4 .or. any(t(5)%v /= -1.0)) error stop 2

do concurrent (i = 1:4)
    u(i) = k(1.0)
    do m = 1, 2
        if (m == 2) u(i) = k(real(i + m))
    end do
end do
print *, size(u(4)%v), u(4)%v
do i = 1, 4
    if (size(u(i)%v) /= 3 .or. any(u(i)%v /= real(i + 2))) error stop 3
end do
if (size(u(5)%v) /= 4 .or. any(u(5)%v /= -2.0)) error stop 4
print *, "ok"
contains
pure function k(s) result(r)
    real, intent(in) :: s
    type(tt) :: r
    allocate(r%v(3))
    r%v = s
end function
end program
