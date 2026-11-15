! Reallocation on assignment to a component in a loop whose bounds inquire
! about the variable: reallocating `p(i)%d` changes neither the size of `p`
! nor its allocation, so `do j = 1, size(p)` does not keep the reallocation
! in the loop, but bounds that read the component itself do.
module realloc_lhs_hoist_12_mod
implicit none
type :: particle_t
    real, allocatable :: r(:), d(:)
end type
contains

subroutine over_particles(p, i, n)
    type(particle_t), intent(inout) :: p(:)
    integer, intent(in) :: i
    integer, intent(out) :: n
    integer :: j
    n = 0
    do j = 1, size(p)
        p(i)%d = p(i)%r - p(j)%r
        n = n + size(p(i)%d)
    end do
end subroutine

! The bounds read the size of the component that the loop reallocates:
! they are evaluated once, before its first reallocation.
subroutine over_component(p, i, trips)
    type(particle_t), intent(inout) :: p(:)
    integer, intent(in) :: i
    integer, intent(out) :: trips
    integer :: j
    trips = 0
    do j = 1, size(p(i)%d)
        p(i)%d = 2*p(i)%r
        trips = trips + 1
    end do
end subroutine

end module

program realloc_lhs_hoist_12
use realloc_lhs_hoist_12_mod
implicit none
type(particle_t), allocatable :: p(:)
integer :: i, n, trips

allocate(p(3))
do i = 1, 3
    p(i)%r = [real(i), 2.0*i, 3.0*i]
end do
call over_particles(p, 2, n)
if (n /= 9) error stop 1
if (size(p(2)%d) /= 3) error stop 2
if (any(abs(p(2)%d - [-1.0, -2.0, -3.0]) > 1e-6)) error stop 3
if (allocated(p(1)%d)) error stop 4

allocate(p(1)%d(2))
call over_component(p, 1, trips)
if (trips /= 2) error stop 5
if (size(p(1)%d) /= 3) error stop 6

! No particles: the loop does not run and the target stays unallocated.
deallocate(p)
allocate(p(0))
call over_particles(p, 1, n)
if (n /= 0) error stop 7

print *, "ok"
end program
