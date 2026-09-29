program gpu_metal_365
! Built with --realloc-lhs-arrays. The offloaded loops write an allocatable
! component in ways the host replays from the loop as it is written: an
! array assigned to the component itself, a structure constructor, one of a
! section with an omitted bound, a write under `select case`, one through
! `associate`, and one whose size and test read a scalar the loop assigns
! first. The host allocates, or allocates again, the component of just the
! elements the loops write.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(6)
type(tt) :: t(3), u(3), w(4), x(4), y(4), z(2)
logical, allocatable :: m(:,:)
integer :: i, k

c = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]

allocate(t(1)%v(1), t(3)%v(1))
t(3)%v = -3.0
do concurrent (i = 1:2)
    t(i)%v = c(i:i+2)
end do
if (size(t(1)%v) /= 3 .or. any(t(1)%v /= c(1:3))) error stop 1
if (size(t(2)%v) /= 3 .or. any(t(2)%v /= c(2:4))) error stop 2
if (size(t(3)%v) /= 1 .or. t(3)%v(1) /= -3.0) error stop 3

do concurrent (i = 1:2)
    u(i) = tt(c(1:i+1))
end do
if (size(u(1)%v) /= 2 .or. size(u(2)%v) /= 3) error stop 4
if (any(u(2)%v /= c(1:3))) error stop 5
if (allocated(u(3)%v)) error stop 6

allocate(m(3, 2))
m = .true.
m(2, 2) = .false.
do concurrent (i = 1:2)
    z(i) = tt(merge(1.0, 0.0, m(:,i)))
end do
if (size(z(1)%v) /= 3 .or. any(z(1)%v /= 1.0)) error stop 16
if (size(z(2)%v) /= 3 .or. z(2)%v(2) /= 0.0 .or. z(2)%v(3) /= 1.0) error stop 17

allocate(w(2)%v(1))
w(2)%v = 9.0
do concurrent (i = 1:4)
    select case (i)
    case (1, 3)
        w(i) = f(c(1:i))
    case default
    end select
end do
if (size(w(1)%v) /= 1 .or. size(w(3)%v) /= 3) error stop 7
if (any(w(3)%v /= c(1:3))) error stop 8
if (size(w(2)%v) /= 1 .or. w(2)%v(1) /= 9.0) error stop 9
if (allocated(w(4)%v)) error stop 10

do concurrent (i = 1:3)
    associate (e => x(i))
        e = f(c(1:2))
    end associate
end do
if (size(x(3)%v) /= 2 .or. any(x(3)%v /= c(1:2))) error stop 11
if (allocated(x(4)%v)) error stop 12

do concurrent (i = 1:4)
    k = i + 1
    if (mod(k, 2) == 0) y(i) = f(c(1:k))
end do
if (size(y(1)%v) /= 2 .or. size(y(3)%v) /= 4) error stop 13
if (any(y(3)%v /= c(1:4))) error stop 14
if (allocated(y(2)%v) .or. allocated(y(4)%v)) error stop 15
print *, "ok"

contains

pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function

end program
