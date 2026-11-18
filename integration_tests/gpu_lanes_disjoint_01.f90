module gpu_lanes_disjoint_01_m
! The lanes of an iteration that read an element of an array before its
! first lane assigns one wait for each other (gpu_vector_lanes.cpp,
! settle_before) unless the two elements differ whatever the values of the
! scalars the kernel is handed: `ss(2*i)` read and `ss(2*i - 1)` assigned
! need no barrier, `ss(i)` read and assigned does.
implicit none
contains
    subroutine disjoint(a, x, ss, y)
        real, intent(in) :: a(:, :), x(:)
        real, intent(inout) :: ss(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(y))
            block
                real :: s, r
                integer :: c
                s = 0
                do c = 1, size(a, 1)
                    s = s + a(c, i)*x(c)
                end do
                r = ss(2*i)
                y(i) = s*r
                ss(2*i - 1) = 0
            end block
        end do
    end subroutine

    subroutine same(a, x, ss, y)
        real, intent(in) :: a(:, :), x(:)
        real, intent(inout) :: ss(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(y))
            block
                real :: s, r
                integer :: c
                s = 0
                do c = 1, size(a, 1)
                    s = s + a(c, i)*x(c)
                end do
                r = ss(i)
                y(i) = s*r
                ss(i) = -1
            end block
        end do
    end subroutine
end module

program gpu_lanes_disjoint_01
use gpu_lanes_disjoint_01_m
implicit none
integer, parameter :: m = 256, n = 40
real :: a(m, n), x(m), ss(2*n), ss0(2*n), y(n), s
integer :: i, j

do j = 1, n
    do i = 1, m
        a(i, j) = mod(i + 3*j, 7) - 3
    end do
end do
do i = 1, m
    x(i) = mod(i, 5) - 2
end do
do i = 1, 2*n
    ss0(i) = mod(i, 4) + 1
end do
ss = ss0
call disjoint(a, x, ss, y)
do i = 1, n
    s = sum(a(:, i)*x)
    if (y(i) /= s*ss0(2*i) .or. ss(2*i - 1) /= 0 .or. ss(2*i) /= ss0(2*i)) then
        print *, "disjoint", i, y(i), s*ss0(2*i)
        error stop 1
    end if
end do
ss = ss0
call same(a, x, ss, y)
do i = 1, n
    s = sum(a(:, i)*x)
    if (y(i) /= s*ss0(i) .or. ss(i) /= -1) then
        print *, "same", i, y(i), s*ss0(i)
        error stop 2
    end if
end do
print *, "ok"
end program
