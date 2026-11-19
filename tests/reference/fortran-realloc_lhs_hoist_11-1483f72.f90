module realloc_lhs_hoist_11_mod
implicit none
type :: particle_t
    real(4), dimension(:), allocatable :: r
end type particle_t

contains

subroutine columns(a, b, c, i, lb)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    real(4), dimension(:, :), intent(in) :: a
    real(4), dimension(:, :), intent(in) :: b
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4), intent(in) :: i
    integer(4) :: j
    integer(4), intent(out) :: lb
    if (1 <= size(a, 2)) then
        reallocate(c(size(b(lbound(b, 1):ubound(b, 1), i), 1)))
    end if
    do j = 1, size(a, 2)
        ! CheckBounds LHS = c, RHS = a(lbound(a, 1):ubound(a, 1), j), b(lbound(b, 1):ubound(b, 1), i), 
        __libasr_index_0_1 = lbound(a, 1)
        __libasr_index_0_2 = lbound(b, 1)
        do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
            c(__libasr_index_0_) = a(__libasr_index_0_1, j) + b(__libasr_index_0_2, i)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
    end do
    lb = lbound(c, 1)
end subroutine columns

integer(4) function first()
    first = 1
end function first

subroutine pairs(p, s)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    real(4), dimension(:), allocatable :: d
    integer(4) :: i
    integer(4) :: j
    type(particle_t), dimension(:), intent(in) :: p
    real(4), intent(out) :: s
    s = real(0, kind=4)
    do i = 1, size(p)
        if (1 <= size(p)) then
            reallocate(d(size(p(i)%r, 1)))
        end if
        do j = 1, size(p)
            ! CheckBounds LHS = d, RHS = p(j)%r, p(i)%r, 
            __libasr_index_0_1 = lbound(p(j)%r, 1)
            __libasr_index_0_2 = lbound(p(i)%r, 1)
            do __libasr_index_0_ = lbound(d, 1), ubound(d, 1)
                d(__libasr_index_0_) = p(j)%r(__libasr_index_0_1) - p(i)%r(__libasr_index_0_2)
                __libasr_index_0_1 = __libasr_index_0_1 + 1
                __libasr_index_0_2 = __libasr_index_0_2 + 1
            end do
            s = s + Sum(d*d)
        end do
    end do
end subroutine pairs

subroutine with_call(a, c, n)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_2
    real(4), dimension(:), intent(in) :: a
    real(4), dimension(:), allocatable, intent(inout) :: c
    integer(4) :: j
    integer(4), intent(out) :: n
    n = 0
    do j = 1, 2
        reallocate(c(size(a(1:j + 1), 1)))
        ! CheckBounds LHS = c, RHS = a(1:j + 1), a(first():first() + j), 
        __libasr_index_0_1 = 1
        __libasr_index_0_2 = first()
        do __libasr_index_0_ = lbound(c, 1), ubound(c, 1)
            c(__libasr_index_0_) = a(__libasr_index_0_1) + a(__libasr_index_0_2)
            __libasr_index_0_1 = __libasr_index_0_1 + 1
            __libasr_index_0_2 = __libasr_index_0_2 + 1
        end do
    end do
    n = size(c)
end subroutine with_call

end module realloc_lhs_hoist_11_mod

program realloc_lhs_hoist_11
use realloc_lhs_hoist_11_mod, only: columns
use realloc_lhs_hoist_11_mod, only: first
use realloc_lhs_hoist_11_mod, only: pairs
use realloc_lhs_hoist_11_mod, only: particle_t
use realloc_lhs_hoist_11_mod, only: with_call
implicit none
integer(4) :: __libasr_index_0_
integer(4) :: __libasr_index_0_1
integer(4) :: __libasr_index_1_
integer(4) :: __libasr_index_1_1
real(4), dimension(4, 3) :: a
real(4), dimension(4, 2) :: b
real(4), dimension(:), allocatable :: c
integer(4) :: i
integer(4) :: lb
integer(4) :: n
type(particle_t), dimension(3) :: p
real(4) :: s
real(4), dimension(5) :: x
do i = 1, 3
    p(i)%r(lbound(p(i)%r, 1) + 0) = Real(i)
    p(i)%r(lbound(p(i)%r, 1) + 1) = 0.00000000e+00
end do
call pairs(p, s)
if (Abs(s - 1.20000000e+01) > 9.99999975e-06) then
    error stop
end if
do __libasr_index_1_ = lbound(a, 2), ubound(a, 2)
    do __libasr_index_0_ = lbound(a, 1), ubound(a, 1)
        a(__libasr_index_0_, __libasr_index_1_) = real(1, kind=4)
    end do
end do
do __libasr_index_1_1 = lbound(b, 2), ubound(b, 2)
    do __libasr_index_0_1 = lbound(b, 1), ubound(b, 1)
        b(__libasr_index_0_1, __libasr_index_1_1) = real(2, kind=4)
    end do
end do
allocate(c(4))
call columns(a, b, c, 1, lb)
if (size(c) /= 4 .or. lb /= 0) then
    error stop
end if
if (Any(c /= real(3, kind=4))) then
    error stop
end if
deallocate(c)
call columns(a, b, c, 2, lb)
if (size(c) /= 4 .or. lb /= 1) then
    error stop
end if
x(1) = 1.00000000e+00
x(2) = 2.00000000e+00
x(3) = 3.00000000e+00
x(4) = 4.00000000e+00
x(5) = 5.00000000e+00
call with_call(x, c, n)
if (n /= 3) then
    error stop
end if
if (Any(c /= [2.00000000e+00, 4.00000000e+00, 6.00000000e+00])) then
    error stop
end if
print *, "ok"
end program realloc_lhs_hoist_11
