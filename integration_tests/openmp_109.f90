program openmp_109
! collapse(n) over a non-rectangular loop nest: the bounds of an inner loop
! depend on the index of an outer one.
implicit none
integer, parameter :: m = 37
integer :: i, j, k, n
integer :: b(m, m), c(m, m, m)
n = m

! Lower triangle: the inner start depends on the outer index
b = 0
!$omp parallel do collapse(2)
do j = 1, n
    do i = j, n
        b(i, j) = b(i, j) + 1
    end do
end do
!$omp end parallel do
call check_lower(b, 0, 1)

! Upper triangle: the inner end depends on the outer index
b = 0
!$omp parallel do collapse(2)
do j = 1, n
    do i = 1, j
        b(i, j) = b(i, j) + 1
    end do
end do
!$omp end parallel do
call check_upper(b, 0, 2)

! Strict lower triangle
b = 0
!$omp parallel do collapse(2)
do j = 1, n
    do i = j + 1, n
        b(i, j) = b(i, j) + 1
    end do
end do
!$omp end parallel do
call check_lower(b, 1, 3)

! Worksharing loop in a parallel region
b = 0
!$omp parallel
!$omp do collapse(2)
do j = 1, n
    do i = 1, n - j
        b(i, j) = b(i, j) + 1
    end do
end do
!$omp end do
!$omp end parallel
do j = 1, n
    do i = 1, n
        if (i <= n - j .and. b(i, j) /= 1) error stop 4
        if (i > n - j .and. b(i, j) /= 0) error stop 5
    end do
end do

! The inner loop depends on the outer index and has a step
b = 0
!$omp parallel do collapse(2)
do j = 1, n
    do i = 2*j, 2*n, 2
        b(i/2, j) = b(i/2, j) + 1
    end do
end do
!$omp end parallel do
call check_lower(b, 0, 6)

! Three loops, only the innermost depends on an outer index
c = 0
!$omp parallel do collapse(3)
do k = 1, n
    do j = 1, n
        do i = k, n
            c(i, j, k) = c(i, j, k) + 1
        end do
    end do
end do
!$omp end parallel do
do k = 1, n
    do j = 1, n
        do i = 1, n
            if (i >= k .and. c(i, j, k) /= 1) error stop 8
            if (i < k .and. c(i, j, k) /= 0) error stop 9
        end do
    end do
end do

! Three loops, the middle one depends on the outermost index
c = 0
!$omp parallel do collapse(3)
do k = 1, n
    do j = k, n
        do i = 1, n
            c(i, j, k) = c(i, j, k) + 1
        end do
    end do
end do
!$omp end parallel do
do k = 1, n
    do j = 1, n
        do i = 1, n
            if (j >= k .and. c(i, j, k) /= 1) error stop 11
            if (j < k .and. c(i, j, k) /= 0) error stop 12
        end do
    end do
end do

! Iterations distributed over teams
b = 0
!$omp teams num_teams(2)
!$omp distribute parallel do collapse(2)
do j = 1, n
    do i = j, n
        b(i, j) = b(i, j) + 1
    end do
end do
!$omp end distribute parallel do
!$omp end teams
call check_lower(b, 0, 10)

print *, "ok"

contains

    subroutine check_lower(a, offset, code)
    integer, intent(in) :: a(:, :), offset, code
    integer :: p, q
    do q = 1, size(a, 2)
        do p = 1, size(a, 1)
            if (p >= q + offset) then
                if (a(p, q) /= 1) error stop code
            else
                if (a(p, q) /= 0) error stop code
            end if
        end do
    end do
    end subroutine

    subroutine check_upper(a, offset, code)
    integer, intent(in) :: a(:, :), offset, code
    integer :: p, q
    do q = 1, size(a, 2)
        do p = 1, size(a, 1)
            if (p <= q - offset) then
                if (a(p, q) /= 1) error stop code
            else
                if (a(p, q) /= 0) error stop code
            end if
        end do
    end do
    end subroutine

end program
