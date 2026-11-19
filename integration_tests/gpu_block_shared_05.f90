module gpu_block_shared_05_m
implicit none
integer, parameter :: n = 128
contains

! An iteration on the lanes of a block, with an array the block shares
! (gpu_vector_lanes.cpp). The sum of squares of the array is accumulated in
! the phase that computes it, as the lanes go, and combined after it. The
! elements of arguments the kernel only reads are read at the start of the
! iteration: `scale(h)` and `bias(h)` of the scalars after the phases, and
! `w(j)` of a phase of one iteration per lane. `y(1, h)` is read after the
! phase that updates `y`, which the kernel assigns, and must see the update.
subroutine phases(x, w, scale, bias, y, z, s, t)
    real, intent(in) :: x(:, :), w(:), scale(:), bias(:)
    real, intent(inout) :: y(:, :)
    real, intent(out) :: z(:, :), s(:), t(:)
    integer :: h
    do concurrent (h = 1:size(x, 2))
        block
            real :: a(n)
            real :: sa, g, u
            integer :: j
            do concurrent (j = 1:n)
                a(j) = x(j, h)*2
                a(j) = a(j) - abs(a(j))/4
                y(j, h) = y(j, h) + a(j)
            end do
            sa = 0
            do j = 1, n
                sa = sa + a(j)*a(j)
            end do
            g = scale(h)*2 + bias(h)
            u = y(1, h)
            do concurrent (j = 1:n)
                z(j, h) = a(j)*g + w(j) + u + sa
            end do
            s(h) = sa
            t(h) = u
        end block
    end do
end subroutine

! Iterations on the lanes of a warp: the sum of squares of what a phase
! assigns to an argument is accumulated in that phase as well.
subroutine norms(x, m, y, s)
    real, intent(in) :: x(:, :)
    integer, intent(in) :: m
    real, intent(out) :: y(:, :), s(:)
    integer :: h
    do concurrent (h = 1:size(x, 2))
        block
            real :: q
            integer :: j
            do concurrent (j = 1:m)
                y(j, h) = x(j, h) + 1
            end do
            q = 0
            do j = 1, m
                q = q + y(j, h)*y(j, h)
            end do
            s(h) = q
        end block
    end do
end subroutine

end module

program gpu_block_shared_05
use gpu_block_shared_05_m
implicit none
integer, parameter :: nh = 7, m = 300
real :: x(n, nh), w(n), scale(nh), bias(nh), y(n, nh), y0(n, nh), z(n, nh)
real :: s(nh), t(nh), a(n), sa, g, u, xx(m, 40), yy(m, 40), ss(40), q
integer :: h, j

do h = 1, nh
    do j = 1, n
        x(j, h) = mod(j + 3*h, 7) - 3
        y(j, h) = mod(2*j + h, 5) - 2
    end do
    scale(h) = h - 4
    bias(h) = mod(h, 3)
end do
do j = 1, n
    w(j) = mod(j, 4) - 1
end do
y0 = y
call phases(x, w, scale, bias, y, z, s, t)
do h = 1, nh
    a = x(:, h)*2
    a = a - abs(a)/4
    sa = sum(a*a)
    g = scale(h)*2 + bias(h)
    u = y0(1, h) + a(1)
    if (s(h) /= sa .or. t(h) /= u) then
        print *, "s, t", h, s(h), sa, t(h), u
        error stop 1
    end if
    do j = 1, n
        if (y(j, h) /= y0(j, h) + a(j)) then
            print *, "y", h, j, y(j, h), y0(j, h) + a(j)
            error stop 2
        end if
        if (z(j, h) /= a(j)*g + w(j) + u + sa) then
            print *, "z", h, j, z(j, h), a(j)*g + w(j) + u + sa
            error stop 3
        end if
    end do
end do

do h = 1, 40
    do j = 1, m
        xx(j, h) = mod(j*h, 9) - 4
    end do
end do
call norms(xx, m, yy, ss)
do h = 1, 40
    q = 0
    do j = 1, m
        q = q + (xx(j, h) + 1)**2
        if (yy(j, h) /= xx(j, h) + 1) error stop 4
    end do
    if (ss(h) /= q) then
        print *, "norms", h, ss(h), q
        error stop 5
    end if
end do
print *, "ok"
end program
