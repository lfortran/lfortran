program gpu_metal_580
! Non-rectangular collapsed `!$omp parallel do` nests offloaded with
! `--gpu-offload-omp-loops`: the bounds of an inner collapsed loop depend on
! the index of an enclosing one, so the iteration space is not the
! rectangle the bounds evaluated before the loop would describe. Every
! element inside the triangle is visited exactly once, none outside it.
implicit none
integer :: i, j, k, n, m
integer :: lower(40, 40), upper(40, 40), strict(40, 40), box(40, 40, 3)
integer :: mid(3, 40, 3)
n = 37
m = 3
lower = 0
upper = 0
strict = 0
box = 0
mid = 0

!$omp parallel do collapse(2)
do j = 1, n
    do i = j, n
        lower(i, j) = lower(i, j) + i + 100 * j
    end do
end do
!$omp end parallel do

!$omp parallel do collapse(2)
do j = 1, n
    do i = 1, j
        upper(i, j) = upper(i, j) + 1
    end do
end do
!$omp end parallel do

!$omp parallel do collapse(2)
do j = 1, n
    do i = j + 1, n
        strict(i, j) = strict(i, j) + 1
    end do
end do
!$omp end parallel do

! Only the innermost loop depends on an enclosing index: the two outer
! loops still form a rectangle.
!$omp parallel do collapse(3)
do k = 1, m
    do j = 1, n
        do i = j, n
            box(i, j, k) = box(i, j, k) + k
        end do
    end do
end do
!$omp end parallel do

! The middle loop depends on the outermost index: only the outermost loop
! is partitioned.
!$omp parallel do collapse(3)
do k = 1, m
    do j = k, n
        do i = 1, m
            mid(i, j, k) = mid(i, j, k) + j
        end do
    end do
end do
!$omp end parallel do

do j = 1, 40
    do i = 1, 40
        if (i >= j .and. j <= n .and. i <= n) then
            if (lower(i, j) /= i + 100 * j) error stop 1
        else
            if (lower(i, j) /= 0) error stop 2
        end if
        if (i <= j .and. j <= n) then
            if (upper(i, j) /= 1) error stop 3
        else
            if (upper(i, j) /= 0) error stop 4
        end if
        if (i > j .and. i <= n) then
            if (strict(i, j) /= 1) error stop 5
        else
            if (strict(i, j) /= 0) error stop 6
        end if
        do k = 1, 3
            if (i >= j .and. i <= n .and. k <= m) then
                if (box(i, j, k) /= k) error stop 7
            else
                if (box(i, j, k) /= 0) error stop 8
            end if
        end do
    end do
end do
k = 0
do j = 1, 40
    do i = 1, 40
        k = k + upper(i, j) + strict(i, j)
    end do
end do
if (k /= n * n) error stop 9
do k = 1, 3
    do j = 1, 40
        do i = 1, 3
            if (j >= k .and. j <= n) then
                if (mid(i, j, k) /= j) error stop 10
            else
                if (mid(i, j, k) /= 0) error stop 11
            end if
        end do
    end do
end do
print *, "ok"
end program
