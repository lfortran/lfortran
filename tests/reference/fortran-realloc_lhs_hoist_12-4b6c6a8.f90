module realloc_lhs_hoist_12_mod
implicit none
type :: particle_t
    real(4), dimension(:), allocatable :: d
    real(4), dimension(:), allocatable :: r
end type particle_t

contains

subroutine over_component(p, i, trips)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4), intent(in) :: i
    integer(4) :: j
    type(particle_t), dimension(:), intent(inout) :: p
    integer(4), intent(out) :: trips
    trips = 0
    do j = 1, size(p(i)%d)
        reallocate(p(i)%d(size(p(i)%r, 1)))
        ! CheckBounds LHS = p(i)%d, RHS = p(i)%r, p(i)%r, 
        __libasr_index_0_1 = lbound(p(i)%r, 1)
        do __libasr_index_0_ = lbound(p(i)%d, 1), ubound(p(i)%d, 1)
            p(i)%d(__libasr_index_0_) = real(2, kind=4)*p(i)%r(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        trips = trips + 1
    end do
end subroutine over_component

subroutine over_particles(p, i, n)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4), intent(in) :: i
    integer(4) :: j
    integer(4), intent(out) :: n
    type(particle_t), dimension(:), intent(inout) :: p
    n = 0
    if (1 <= size(p)) then
        reallocate(p(i)%d(size(p(i)%r, 1)))
    end if
    do j = 1, size(p)
        ! CheckBounds LHS = p(i)%d, RHS = p(i)%r, p(j)%r, 
        __libasr_index_0_1 = lbound(p(i)%r, 1)
        __libasr_index_0_2 = lbound(p(j)%r, 1)
        do __libasr_index_0_ = lbound(p(i)%d, 1), ubound(p(i)%d, 1)
            p(i)%d(__libasr_index_0_) = p(i)%r(__libasr_index_0_1) - p(j)%r(__libasr_index_0_2)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
        n = n + size(p(i)%d)
    end do
end subroutine over_particles

end module realloc_lhs_hoist_12_mod

program realloc_lhs_hoist_12
use realloc_lhs_hoist_12_mod, only: over_component
use realloc_lhs_hoist_12_mod, only: over_particles
use realloc_lhs_hoist_12_mod, only: particle_t
implicit none
integer(4) :: i
integer(4) :: n
type(particle_t), dimension(:), allocatable :: p
integer(4) :: trips
allocate(p(3))
do i = 1, 3
    p(i)%r(lbound(p(i)%r, 1) + 0) = Real(i)
    p(i)%r(lbound(p(i)%r, 1) + 1) = 2.00000000e+00*real(i, kind=4)
    p(i)%r(lbound(p(i)%r, 1) + 2) = 3.00000000e+00*real(i, kind=4)
end do
call over_particles(p, 2, n)
if (n /= 9) then
    error stop
end if
if (size(p(2)%d) /= 3) then
    error stop
end if
if (Any(Abs(p(2)%d - [-1.00000000e+00, -2.00000000e+00, -3.00000000e+00]) > 9.99999997e-07)) then
    error stop
end if
if (Allocated(p(1)%d)) then
    error stop
end if
allocate(p(1)%d(2))
call over_component(p, 1, trips)
if (trips /= 2) then
    error stop
end if
if (size(p(1)%d) /= 3) then
    error stop
end if
deallocate(p)
allocate(p(0))
call over_particles(p, 1, n)
if (n /= 0) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_12
