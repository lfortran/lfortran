module realloc_lhs_hoist_10_mod
implicit none
type :: particle_t
    real(4), dimension(:), allocatable :: d
    real(4), dimension(:), allocatable :: f
    real(4), dimension(:), allocatable :: r
end type particle_t

contains

subroutine moving_subscript(p, sizes)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: j
    integer(4) :: k
    type(particle_t), dimension(:), intent(inout) :: p
    integer(4), dimension(:), intent(out) :: sizes
    k = 1
    do j = 1, size(p)
        reallocate(p(k)%d(size(p(k)%r, 1)))
        ! CheckBounds LHS = p(k)%d, RHS = p(k)%r, p(k)%r, 
        __libasr_index_0_1 = lbound(p(k)%r, 1)
        do __libasr_index_0_ = lbound(p(k)%d, 1), ubound(p(k)%d, 1)
            p(k)%d(__libasr_index_0_) = real(2, kind=4)*p(k)%r(__libasr_index_0_1)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
        end do
        sizes(j) = size(p(k)%d)
        k = k + 1
    end do
end subroutine moving_subscript

subroutine pair_component(p, i)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4), intent(in) :: i
    integer(4) :: j
    type(particle_t), dimension(:), intent(inout) :: p
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
        ! CheckBounds LHS = p(i)%f, RHS = p(i)%f, p(i)%d, 
        __libasr_index_0_4 = lbound(p(i)%f, 1)
        __libasr_index_0_5 = lbound(p(i)%d, 1)
        do __libasr_index_0_3 = lbound(p(i)%f, 1), ubound(p(i)%f, 1)
            p(i)%f(__libasr_index_0_3) = p(i)%f(__libasr_index_0_4) + p(i)%d(__libasr_index_0_5)
            __libasr_index_0_4 = __libasr_index_0_4 + 1
            __libasr_index_0_5 = __libasr_index_0_5 + 1
        end do
    end do
end subroutine pair_component

subroutine pair_local(p, i, s)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    real(4), dimension(:), allocatable :: d
    integer(4), intent(in) :: i
    integer(4) :: j
    type(particle_t), dimension(:), intent(inout) :: p
    real(4), intent(out) :: s
    s = real(0, kind=4)
    if (1 <= size(p)) then
        reallocate(d(size(p(i)%r, 1)))
    end if
    do j = 1, size(p)
        ! CheckBounds LHS = d, RHS = p(i)%r, p(j)%r, 
        __libasr_index_0_1 = lbound(p(i)%r, 1)
        __libasr_index_0_2 = lbound(p(j)%r, 1)
        do __libasr_index_0_ = lbound(d, 1), ubound(d, 1)
            d(__libasr_index_0_) = p(i)%r(__libasr_index_0_1) - p(j)%r(__libasr_index_0_2)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
        s = s + Sum(d)
    end do
end subroutine pair_local

end module realloc_lhs_hoist_10_mod

program realloc_lhs_hoist_10
use realloc_lhs_hoist_10_mod, only: moving_subscript
use realloc_lhs_hoist_10_mod, only: pair_component
use realloc_lhs_hoist_10_mod, only: pair_local
use realloc_lhs_hoist_10_mod, only: particle_t
implicit none
integer(4) :: __libasr_index_0_
integer(4) :: __libasr_lbound_
integer(4) :: __libasr_lbound_1
integer(4) :: i
type(particle_t), dimension(3) :: p
real(4) :: s
integer(4), dimension(3) :: sizes
do i = 1, 3
    p(i)%r(lbound(p(i)%r, 1) + 0) = Real(i)
    p(i)%r(lbound(p(i)%r, 1) + 1) = 2.00000000e+00*real(i, kind=4)
    p(i)%r(lbound(p(i)%r, 1) + 2) = 3.00000000e+00*real(i, kind=4)
    allocate(p(i)%f(3))
    do __libasr_index_0_ = lbound(p(i)%f, 1), ubound(p(i)%f, 1)
        p(i)%f(__libasr_index_0_) = real(0, kind=4)
    end do
end do
call pair_component(p, 2)
if (.not. Allocated(p(2)%d)) then
    error stop
end if
if (size(p(2)%d) /= 3) then
    error stop
end if
if (Any(Abs(p(2)%d - [-1.00000000e+00, -2.00000000e+00, -3.00000000e+00]) > 9.99999997e-07)) then
    error stop
end if
if (Any(Abs(p(2)%f) > 9.99999997e-07)) then
    error stop
end if
if (Allocated(p(1)%d) .or. Allocated(p(3)%d)) then
    error stop
end if
deallocate(p(2)%d)
allocate(p(2)%d(7))
call pair_component(p, 2)
if (size(p(2)%d) /= 3) then
    error stop
end if
call pair_local(p, 3, s)
if (Abs(s - 1.80000000e+01) > 9.99999975e-06) then
    error stop
end if
__libasr_lbound_ = lbound(p(1)%r, 1)
p(1)%r(__libasr_lbound_ + 0) = 1.00000000e+00
__libasr_lbound_1 = lbound(p(3)%r, 1)
p(3)%r(__libasr_lbound_1 + 0) = 1.00000000e+00
p(3)%r(__libasr_lbound_1 + 1) = 2.00000000e+00
call moving_subscript(p, sizes)
if (Any(sizes /= [1, 3, 2])) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_10
