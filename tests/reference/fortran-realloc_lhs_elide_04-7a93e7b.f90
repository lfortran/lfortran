module realloc_lhs_elide_04_mod
implicit none

contains

subroutine check(u, t)
    real(4), dimension(:), intent(in) :: t
    real(4), dimension(:), intent(in) :: u
    if (size(u) /= 3 .or. Any(u /= [4.00000000e+00, 5.00000000e+00, 6.00000000e+00])) then
        error stop
    end if
    if (size(t) /= 3 .or. Any(t /= [5.00000000e+00, 6.00000000e+00, 7.00000000e+00])) then
        error stop
    end if
end subroutine check

subroutine step(x, v, dt)
    integer(4) :: __libasr_index_0_
    integer(4) :: __libasr_index_0_1
    integer(4) :: __libasr_index_0_10
    integer(4) :: __libasr_index_0_11
    integer(4) :: __libasr_index_0_12
    integer(4) :: __libasr_index_0_13
    integer(4) :: __libasr_index_0_14
    integer(4) :: __libasr_index_0_15
    integer(4) :: __libasr_index_0_16
    integer(4) :: __libasr_index_0_17
    integer(4) :: __libasr_index_0_18
    integer(4) :: __libasr_index_0_2
    integer(4) :: __libasr_index_0_3
    integer(4) :: __libasr_index_0_4
    integer(4) :: __libasr_index_0_5
    integer(4) :: __libasr_index_0_6
    integer(4) :: __libasr_index_0_7
    integer(4) :: __libasr_index_0_8
    integer(4) :: __libasr_index_0_9
    real(4), intent(in) :: dt
    real(4), dimension(:), allocatable :: t
    real(4), dimension(:), allocatable :: u
    real(4), dimension(:), allocatable, intent(inout) :: v
    real(4), dimension(:), allocatable, intent(inout) :: x
    allocate(t(size(v, 1)))
    ! CheckBounds LHS = t, RHS = v, 
    __libasr_index_0_1 = lbound(v, 1)
    do __libasr_index_0_ = lbound(t, 1), ubound(t, 1)
        t(__libasr_index_0_) = v(__libasr_index_0_1)
        __libasr_index_0_1 = __libasr_index_0_1 + 1
    end do
    ! CheckBounds LHS = x, RHS = x, v, v, 
    __libasr_index_0_3 = lbound(x, 1)
    __libasr_index_0_4 = lbound(v, 1)
    do __libasr_index_0_2 = lbound(x, 1), ubound(x, 1)
        x(__libasr_index_0_2) = x(__libasr_index_0_3) + dt*v(__libasr_index_0_4)
        __libasr_index_0_3 = __libasr_index_0_3 + 1
        __libasr_index_0_4 = __libasr_index_0_4 + 1
    end do
    ! CheckBounds LHS = v, RHS = v, 
    __libasr_index_0_6 = lbound(v, 1)
    do __libasr_index_0_5 = lbound(v, 1), ubound(v, 1)
        v(__libasr_index_0_5) = Abs(v(__libasr_index_0_6))
        __libasr_index_0_6 = __libasr_index_0_6 + 1
    end do
    ! CheckBounds LHS = v, RHS = t, 
    __libasr_index_0_8 = lbound(t, 1)
    do __libasr_index_0_7 = lbound(v, 1), ubound(v, 1)
        v(__libasr_index_0_7) = t(__libasr_index_0_8)
        __libasr_index_0_8 = __libasr_index_0_8 + 1
    end do
    reallocate(x((ubound(x, 1) - 2)/1 + 1))
    __libasr_index_0_10 = 2
    do __libasr_index_0_9 = lbound(x, 1), ubound(x, 1)
        x(__libasr_index_0_9) = x(__libasr_index_0_10)
        __libasr_index_0_10 = __libasr_index_0_10 + 1
    end do
    allocate(u(size(x, 1)))
    ! CheckBounds LHS = u, RHS = x, 
    __libasr_index_0_12 = lbound(x, 1)
    do __libasr_index_0_11 = lbound(u, 1), ubound(u, 1)
        u(__libasr_index_0_11) = x(__libasr_index_0_12)
        __libasr_index_0_12 = __libasr_index_0_12 + 1
    end do
    ! CheckBounds LHS = u, RHS = x, x, 
    __libasr_index_0_14 = lbound(x, 1)
    do __libasr_index_0_13 = lbound(u, 1), ubound(u, 1)
        u(__libasr_index_0_13) = real(2, kind=4)*x(__libasr_index_0_14)
        __libasr_index_0_14 = __libasr_index_0_14 + 1
    end do
    reallocate(u(size(v, 1)))
    ! CheckBounds LHS = u, RHS = v, 
    __libasr_index_0_16 = lbound(v, 1)
    do __libasr_index_0_15 = lbound(u, 1), ubound(u, 1)
        u(__libasr_index_0_15) = v(__libasr_index_0_16)
        __libasr_index_0_16 = __libasr_index_0_16 + 1
    end do
    deallocate(t)
    allocate(t(size(u, 1)))
    ! CheckBounds LHS = t, RHS = u, u, 
    __libasr_index_0_18 = lbound(u, 1)
    do __libasr_index_0_17 = lbound(t, 1), ubound(t, 1)
        t(__libasr_index_0_17) = u(__libasr_index_0_18) + real(1, kind=4)
        __libasr_index_0_18 = __libasr_index_0_18 + 1
    end do
    call check(u, t)
end subroutine step

end module realloc_lhs_elide_04_mod

program realloc_lhs_elide_04
use realloc_lhs_elide_04_mod, only: check
use realloc_lhs_elide_04_mod, only: step
implicit none
integer(4) :: __libasr_lbound_
integer(4) :: __libasr_lbound_1
real(4), dimension(:), allocatable :: v
real(4), dimension(:), allocatable :: x
__libasr_lbound_ = lbound(x, 1)
x(__libasr_lbound_ + 0) = 1.00000000e+00
x(__libasr_lbound_ + 1) = 2.00000000e+00
x(__libasr_lbound_ + 2) = 3.00000000e+00
__libasr_lbound_1 = lbound(v, 1)
v(__libasr_lbound_1 + 0) = 4.00000000e+00
v(__libasr_lbound_1 + 1) = 5.00000000e+00
v(__libasr_lbound_1 + 2) = 6.00000000e+00
call step(x, v, 5.00000000e-01)
if (size(x) /= 2 .or. Any(x /= [4.50000000e+00, 6.00000000e+00])) then
    error stop
end if
if (size(v) /= 3 .or. Any(v /= [4.00000000e+00, 5.00000000e+00, 6.00000000e+00])) then
    error stop
end if
print *, "ok"
end program realloc_lhs_elide_04
