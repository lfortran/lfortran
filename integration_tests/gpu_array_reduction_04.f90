module gpu_array_reduction_04_m
implicit none
contains
    ! ss(1) = sum(x**2) afterwards, as the residual update of a transformer
    ! computes it for the RMSNorm after it: the host does not assign ss(1)
    ! before the loop, which would wait for the launches that use ss.
    subroutine squares(x, ss)
        real, intent(in) :: x(:)
        real, intent(inout) :: ss(:)
        integer :: i
        ss(1) = 0
        do concurrent (i = 1:size(x)) reduce(+: ss)
            ss(1) = ss(1) + x(i)*x(i)
        end do
    end subroutine

    subroutine scale(x, ss, y)
        real, intent(in) :: x(:), ss(:)
        real, intent(inout) :: y(:)
        integer :: i
        do concurrent (i = 1:size(x))
            y(i) = x(i) * ss(1)
        end do
    end subroutine
end module

program gpu_array_reduction_04
! The statements right before a loop that assign constants to elements of an
! array the loop reduces into are not run by the host: the combination on
! the device starts from the constants (gpu_take_reduction_initializers),
! and the host assigns them only when the loop has no iterations. The sizes
! include none, and more than the iterations of a launch that runs in
! chunks (the _chunks test).
use gpu_array_reduction_04_m, only: squares, scale
implicit none
integer :: sizes(6), q, n, i, k
real(8), allocatable :: a(:)
real, allocatable :: x(:), y(:)
real(8) :: s(3), rs(3)
integer :: m(2), rm(2)
real :: ss(1024)

sizes = [0, 1, 33, 1000, 262145, 1000003]
do q = 1, size(sizes)
    n = sizes(q)
    allocate(a(n), x(n), y(n))
    do i = 1, n
        a(i) = real(mod(i, 13) - 6, 8)
        x(i) = real(mod(i, 5) - 2)
    end do

    s = 7
    s(1) = 0
    s(3) = 100
    do concurrent (i = 1:n) reduce(+: s)
        s(1) = s(1) + a(i)
        s(3) = s(3) + 1
    end do
    rs = [0.0_8, 7.0_8, 100.0_8]
    do i = 1, n
        rs(1) = rs(1) + a(i)
        rs(3) = rs(3) + 1
    end do
    if (any(s /= rs)) then
        print *, "sum", n, s, rs
        error stop 1
    end if

    m = 5
    m(2) = -huge(1)
    do concurrent (i = 1:n) reduce(max: m)
        m(2) = max(m(2), int(a(i)))
    end do
    rm = [5, -huge(1)]
    do i = 1, n
        rm(2) = max(rm(2), int(a(i)))
    end do
    if (any(m /= rm)) then
        print *, "max", n, m, rm
        error stop 2
    end if

    ! Twice, each read by the next loop on the device.
    ss = 3
    do k = 1, 2
        call squares(x, ss)
        call scale(x, ss, y)
    end do
    if (ss(1) /= sum(x*x) .or. ss(2) /= 3 .or. any(y /= x * sum(x*x))) then
        print *, "squares", n, ss(1:2), sum(x*x)
        error stop 3
    end if
    deallocate(a, x, y)
end do
print *, "ok"
end program
