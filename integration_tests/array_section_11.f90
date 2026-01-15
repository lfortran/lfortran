program array_section_11
    ! Test that variable-size allocas in loops don't cause stack overflow.
    ! When array sections are passed to subroutines inside loops, variable-size
    ! allocas are created. These should NOT be hoisted to entry block to avoid
    ! stack growth on each iteration.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer, parameter :: n = 4
    integer, parameter :: iterations = 100000
    real(dp) :: a(n, n), b(n, n)
    integer :: i, j

    do j = 1, n
        do i = 1, n
            a(i, j) = real(i + j, dp)
            b(i, j) = 0.0d0
        end do
    end do

    do i = 1, iterations
        call consume(a(:, 1), b(:, 2))
    end do

    if (abs(b(1, 2) - real(iterations, dp) * a(1, 1)) > 1.0d-6) error stop

contains

    subroutine consume(x, y)
        real(dp), intent(in) :: x(:)
        real(dp), intent(inout) :: y(:)
        y(1) = y(1) + x(1)
    end subroutine

end program
