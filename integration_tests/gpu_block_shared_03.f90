module gpu_block_shared_03_m
implicit none
integer, parameter :: n = 64
contains

pure real function total(v)
    real, intent(in) :: v(n)
    integer :: j
    total = 0
    do j = 1, n
        total = total + v(j)
    end do
end function

! Arrays of an iteration that stay private to each lane
! (gpu_vector_lanes.cpp): one of fewer elements than a warp has lanes, one
! handed whole to a function, one declared in a block inside a loop of the
! iteration (an array per iteration of that loop), and one that only the
! first lane assigns, element after element, since each element depends on
! the one before. The arrays an inner `do concurrent` assigns at the
! subscript its iterations tell apart still live in the shared memory of
! the block.
subroutine mixed(x, y, z)
    real, intent(in) :: x(:, :)
    real, intent(out) :: y(:, :), z(:)
    integer :: h
    do concurrent (h = 1:size(x, 2))
        block
            real :: small(8), whole(n), chain(n), a(n)
            real :: t, u
            integer :: i, j, k
            do concurrent (j = 1:8)
                small(j) = x(j, h)*j
                small(j) = small(j) + abs(small(j))
            end do
            do concurrent (j = 1:n)
                whole(j) = x(j, h) + 2
            end do
            do concurrent (j = 1:n)
                a(j) = x(j, h) - 1
                a(j) = a(j)*abs(a(j))
            end do
            chain(1) = x(1, h)
            do j = 2, n
                chain(j) = chain(j - 1) + x(j, h)
            end do
            u = 0
            do k = 1, 2
                block
                    real :: tmp(n)
                    do concurrent (j = 1:n)
                        tmp(j) = a(j)*k
                        tmp(j) = tmp(j) - k
                    end do
                    do j = 1, n
                        u = u + tmp(j)
                    end do
                end block
            end do
            t = total(whole)
            do concurrent (i = 1:n)
                y(i, h) = chain(i) + small(mod(i - 1, 8) + 1) + a(i) + t
            end do
            z(h) = u
        end block
    end do
end subroutine

end module

program gpu_block_shared_03
use gpu_block_shared_03_m
implicit none
integer :: sizes(2), k, nh, h, i, j
real, allocatable :: x(:, :), y(:, :), z(:)
real :: small(8), whole(n), chain(n), a(n), t, u

sizes = [4, 2000]
do k = 1, size(sizes)
    nh = sizes(k)
    allocate(x(n, nh), y(n, nh), z(nh))
    do h = 1, nh
        do j = 1, n
            x(j, h) = mod(5*j + 3*h, 9) - 4
        end do
    end do
    call mixed(x, y, z)
    do h = 1, nh
        do j = 1, 8
            small(j) = x(j, h)*j
            small(j) = small(j) + abs(small(j))
        end do
        whole = x(:, h) + 2
        a = x(:, h) - 1
        a = a*abs(a)
        chain(1) = x(1, h)
        do j = 2, n
            chain(j) = chain(j - 1) + x(j, h)
        end do
        u = 3*sum(a) - 3*n
        t = sum(whole)
        if (z(h) /= u) then
            print *, "z", nh, h, z(h), u
            error stop 1
        end if
        do i = 1, n
            if (y(i, h) /= chain(i) + small(mod(i - 1, 8) + 1) + a(i) + t) then
                print *, "y", nh, h, i, y(i, h), &
                    chain(i) + small(mod(i - 1, 8) + 1) + a(i) + t
                error stop 2
            end if
        end do
    end do
    deallocate(x, y, z)
end do
print *, "ok"
end program
