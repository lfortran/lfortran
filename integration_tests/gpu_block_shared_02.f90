module gpu_block_shared_02_m
implicit none
integer, parameter :: n = 96, m = 8
contains

! The arrays of an iteration live in the shared memory of the block that
! runs it (gpu_vector_lanes.cpp), and the lanes of the block go through the
! steps of a sequential loop together. In each step every lane reads an
! element of `a` that the phase after it assigns, and the lanes that run
! ahead must wait for the others before they assign it; the sum at the end
! of a step reads `a`, which the next step assigns again. An integer array
! of two dimensions is filled by a `do concurrent` over both of its indices,
! and a phase reads its elements across the iterations that wrote them.
subroutine relax(x, steps, y, s, r)
    real, intent(in) :: x(:, :)
    integer, intent(in) :: steps
    real, intent(out) :: y(:, :), s(:)
    integer, intent(out) :: r(:)
    integer :: h
    do concurrent (h = 1:size(x, 2))
        block
            real :: a(n)
            integer :: p(m, m)
            real :: first, t
            integer :: i, j, k, q
            do concurrent (j = 1:n)
                a(j) = x(j, h)
            end do
            do k = 1, steps
                first = a(1)
                do concurrent (j = 1:n)
                    a(j) = a(j) - first + k
                end do
                t = 0
                do j = 1, n
                    t = t + a(j)
                end do
                s(h) = t
            end do
            do concurrent (i = 1:m, j = 1:m)
                p(i, j) = i*j + h
            end do
            q = 0
            do concurrent (i = 1:m)
                y(i, h) = p(m + 1 - i, i) + a(i)
            end do
            do j = 1, m
                q = q + p(j, 1)
            end do
            r(h) = q
        end block
    end do
end subroutine

end module

program gpu_block_shared_02
use gpu_block_shared_02_m
implicit none
integer :: sizes(2), k, nh, h, i, j, q, r_ref
real, allocatable :: x(:, :), y(:, :), s(:)
integer, allocatable :: r(:)
real :: a(n), first, t
integer :: p(m, m)

sizes = [3, 700]
do k = 1, size(sizes)
    nh = sizes(k)
    allocate(x(n, nh), y(m, nh), s(nh), r(nh))
    do h = 1, nh
        do j = 1, n
            x(j, h) = mod(7*j + h, 11) - 5
        end do
    end do
    call relax(x, 3, y, s, r)
    do h = 1, nh
        a = x(:, h)
        do q = 1, 3
            first = a(1)
            a = a - first + q
            t = sum(a)
        end do
        if (s(h) /= t) then
            print *, "s", nh, h, s(h), t
            error stop 1
        end if
        do j = 1, m
            do i = 1, m
                p(i, j) = i*j + h
            end do
        end do
        do i = 1, m
            if (y(i, h) /= p(m + 1 - i, i) + a(i)) then
                print *, "y", nh, h, i, y(i, h), p(m + 1 - i, i) + a(i)
                error stop 2
            end if
        end do
        r_ref = sum(p(:, 1))
        if (r(h) /= r_ref) then
            print *, "r", nh, h, r(h), r_ref
            error stop 3
        end if
    end do
    deallocate(x, y, s, r)
end do
print *, "ok"
end program
