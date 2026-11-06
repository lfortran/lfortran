! Program arrays used in OpenMP parallel regions passed to internal procedures
! with explicit-shape, fixed-size and assumed-shape dummy arguments.
program openmp_110
    implicit none
    real :: a(8), c(2, 3)
    integer :: i
    call explicit_shape(a, 8)
    if (abs(sum(a) - 36.0) > 1e-4) error stop 1
    call fixed_size(a)
    if (abs(sum(a) - 72.0) > 1e-4) error stop 2
    call assumed_shape(a)
    if (abs(sum(a) - 108.0) > 1e-4) error stop 3
    if (abs(total(a, 8) - 108.0) > 1e-4) error stop 4
    call fill2(c, 2, 3)
    if (abs(sum(c) - 21.0) > 1e-4) error stop 5
    !$omp parallel do
    do i = 1, 8
        a(i) = 2*i
    end do
    !$omp end parallel do
    !$omp parallel do
    do i = 1, 3
        c(:, i) = c(:, i) + 1
    end do
    !$omp end parallel do
    if (abs(sum(a) - 72.0) > 1e-4) error stop 6
    if (abs(total(a, 8) - 72.0) > 1e-4) error stop 7
    if (abs(sum(c) - 27.0) > 1e-4) error stop 8
    print *, sum(a), sum(c)
contains
    subroutine explicit_shape(b, n)
        integer, intent(in) :: n
        real, intent(out) :: b(n)
        integer :: j
        do j = 1, n
            b(j) = j
        end do
    end subroutine
    subroutine fixed_size(b)
        real, intent(inout) :: b(8)
        b = 2*b
    end subroutine
    subroutine assumed_shape(b)
        real, intent(inout) :: b(:)
        b = 1.5*b
    end subroutine
    real function total(b, n)
        integer, intent(in) :: n
        real, intent(in) :: b(n)
        total = sum(b)
    end function
    subroutine fill2(b, m, n)
        integer, intent(in) :: m, n
        real, intent(out) :: b(m, n)
        integer :: j, k
        do k = 1, n
            do j = 1, m
                b(j, k) = j + (k - 1)*m
            end do
        end do
    end subroutine
end program
