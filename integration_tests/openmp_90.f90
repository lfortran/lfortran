module openmp_90_m
implicit none
contains
    subroutine work(x)
    integer, intent(out) :: x
    integer :: a(100), i
    !$omp parallel do
    do i = 1, 100
        a(i) = 2*i
    end do
    !$omp end parallel do
    x = sum(a)
    end subroutine

    integer function scaled_sum(k)
    integer, intent(in) :: k
    integer :: j, s
    s = 0
    !$omp parallel do reduction(+:s)
    do j = 1, 10
        s = s + k
    end do
    !$omp end parallel do
    scaled_sum = s
    end function
end module

program openmp_90
use openmp_90_m, only: work, scaled_sum
implicit none
integer :: x, i, b(4), c(4)
call work(x)
if (x /= 10100) error stop
!$omp parallel do
do i = 1, 4
    b(i) = scaled_sum(i)
end do
!$omp end parallel do
if (b(1) /= 10 .or. b(2) /= 20 .or. b(3) /= 30 .or. b(4) /= 40) error stop
!$omp parallel do
do i = 1, 4
    c(i) = local_sum(i)
end do
!$omp end parallel do
if (c(1) /= 10 .or. c(2) /= 20 .or. c(3) /= 30 .or. c(4) /= 40) error stop
print *, x, b, c
contains
    integer function local_sum(k)
    integer, intent(in) :: k
    integer :: j, s
    s = 0
    !$omp parallel do reduction(+:s)
    do j = 1, 10
        s = s + k
    end do
    !$omp end parallel do
    local_sum = s
    end function
end program
