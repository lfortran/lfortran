module openmp_91_m
implicit none
contains
    subroutine work(x)
    integer, intent(out) :: x
    integer :: a(100), i
    !$omp parallel do
    do i = 1, 100
        a(i) = 3*i
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
