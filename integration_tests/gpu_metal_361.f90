program gpu_metal_361
! Built without --realloc-lhs-arrays, so the program allocates every
! component the offloaded loops write, with its right size, and bounds
! checking must report nothing:
! * the first loop writes only the odd elements, under an `if`; the even
!   ones are not allocated, or allocated with another size;
! * in the second the called function allocates the component with one of
!   two sizes, so the host cannot tell the size before the loop runs.
implicit none
type tt
    real, allocatable :: v(:)
end type
real :: c(5)
type(tt) :: t(4), s(3)
integer :: i, n

c = [1.0, 2.0, 3.0, 4.0, 5.0]
n = 3

allocate(t(1)%v(n), t(3)%v(n))
allocate(t(2)%v(1))
t(2)%v = 9.0
do concurrent (i = 1:4)
    if (mod(i, 2) == 1) t(i) = f(c(1:n))
end do
if (any(t(1)%v /= c(1:3)) .or. any(t(3)%v /= c(1:3))) error stop 1
if (size(t(2)%v) /= 1 .or. t(2)%v(1) /= 9.0) error stop 2
if (allocated(t(4)%v)) error stop 3

allocate(s(1)%v(1), s(2)%v(5), s(3)%v(5))
do concurrent (i = 1:3)
    s(i) = h(c, i)
end do
if (size(s(1)%v) /= 1 .or. s(1)%v(1) /= 1.0) error stop 4
if (size(s(2)%v) /= 5 .or. any(s(2)%v /= c)) error stop 5
if (size(s(3)%v) /= 5 .or. any(s(3)%v /= c)) error stop 6
print *, "ok"

contains

pure function f(a) result(r)
    real, intent(in) :: a(:)
    type(tt) :: r
    r%v = a
end function

pure function h(a, k) result(r)
    real, intent(in) :: a(:)
    integer, intent(in) :: k
    type(tt) :: r
    if (k > 1) then
        allocate(r%v(5))
    else
        allocate(r%v(1))
    end if
    r%v = a(1:size(r%v))
end function

end program
