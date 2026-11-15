! Reallocation on assignment in loops to and from components of array
! elements, `p(i)%d = p(i)%r - p(j)%r` in a loop over `j`: the element is
! the same in every iteration when its subscripts do not change, so the
! reallocation moves before the loop.
module realloc_lhs_hoist_10_mod
implicit none
type :: particle_t
    real, allocatable :: r(:), f(:), d(:)
end type
contains

! The target and the source of the shape are components of p(i).
subroutine pair_component(p, i)
    type(particle_t), intent(inout) :: p(:)
    integer, intent(in) :: i
    integer :: j
    do j = 1, size(p)
        p(i)%d = p(i)%r - p(j)%r
        p(i)%f = p(i)%f + p(i)%d
    end do
end subroutine

! A local work array with the shape of a component of p(i).
subroutine pair_local(p, i, s)
    type(particle_t), intent(inout) :: p(:)
    integer, intent(in) :: i
    real, intent(out) :: s
    real, allocatable :: d(:)
    integer :: j
    s = 0
    do j = 1, size(p)
        d = p(i)%r - p(j)%r
        s = s + sum(d)
    end do
end subroutine

! The subscript changes in the loop: the reallocation stays in it.
subroutine moving_subscript(p, sizes)
    type(particle_t), intent(inout) :: p(:)
    integer, intent(out) :: sizes(:)
    integer :: j, k
    k = 1
    do j = 1, size(p)
        p(k)%d = 2*p(k)%r
        sizes(j) = size(p(k)%d)
        k = k + 1
    end do
end subroutine

end module

program realloc_lhs_hoist_10
use realloc_lhs_hoist_10_mod
implicit none
type(particle_t) :: p(3)
integer :: i, sizes(3)
real :: s

do i = 1, 3
    p(i)%r = [real(i), 2.0*i, 3.0*i]
    allocate(p(i)%f(3))
    p(i)%f = 0
end do

! p(2)%d is unallocated before the loop.
call pair_component(p, 2)
if (.not. allocated(p(2)%d)) error stop 1
if (size(p(2)%d) /= 3) error stop 2
! d = r2 - r3 in the last iteration
if (any(abs(p(2)%d - [-1.0, -2.0, -3.0]) > 1e-6)) error stop 3
! f = sum over j of (r2 - rj) = 3*r2 - (r1 + r2 + r3) = 0
if (any(abs(p(2)%f) > 1e-6)) error stop 4
if (allocated(p(1)%d) .or. allocated(p(3)%d)) error stop 5
! Another shape is reallocated.
deallocate(p(2)%d)
allocate(p(2)%d(7))
call pair_component(p, 2)
if (size(p(2)%d) /= 3) error stop 6

call pair_local(p, 3, s)
! sum over j of sum(r3 - rj) = 3*sum(r3) - sum of all = 3*18 - 36
if (abs(s - 18.0) > 1e-5) error stop 7

p(1)%r = [1.0]
p(3)%r = [1.0, 2.0]
call moving_subscript(p, sizes)
if (any(sizes /= [1, 3, 2])) error stop 8

print *, "ok"
end program
