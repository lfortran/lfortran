program gpu_array_statements_realloc_01
! With reallocation on assignment, `b = a` may change the shape of the
! allocatable `b`, which only the host can do, unless the statements before
! show that `a` and `b` have the same shape: one ALLOCATE statement gave
! them the same bounds and nothing since changed the allocation of either.
! Then the assignment is offloaded like the loops around it. Arrays given
! different bounds, reallocated in between, or passed to a call in between
! keep their assignments on the host. The results are checked either way.
implicit none
real, allocatable :: a(:, :), b(:, :), c(:, :), d(:, :), e(:, :)
integer :: n, i, j, pass
do pass = 1, 2
    n = 3 + pass
    allocate(a(n, 4), b(n, 4), c(n + 1, 4), d(n, 4), e(n, 4))
    do j = 1, 4
        do i = 1, n
            a(i, j) = i + 10*j + pass
        end do
    end do
    ! Same bounds in one ALLOCATE: offloaded.
    b = a
    ! Different bounds: c is reallocated on the host.
    c = a
    ! Reallocated in between: stays on the host.
    deallocate(d)
    allocate(d(n + 2, 4))
    d = a
    ! A call in between could have changed the allocation of e.
    call touch(e)
    e = a
    do concurrent (j = 1:4, i = 1:n)
        b(i, j) = b(i, j) + c(i, j) + d(i, j) + e(i, j)
    end do
    if (any(shape(c) /= [n, 4])) error stop 1
    if (any(shape(d) /= [n, 4])) error stop 2
    if (any(b /= 4*a)) error stop 3
    deallocate(a, b, c, d, e)
end do
print *, "ok"
contains
subroutine touch(x)
    real, allocatable, intent(inout) :: x(:, :)
    x = 0
end subroutine
end program
