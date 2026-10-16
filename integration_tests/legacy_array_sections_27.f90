program legacy_array_sections_27
    ! An element of an allocatable array passed to an explicit-shape dummy
    ! is sequence associated with the elements that follow it in array
    ! element order (#15332).
    implicit none
    real, allocatable :: a(:,:,:)
    integer, allocatable :: b(:,:)
    integer :: i
    allocate(a(1,2,2), source=1.)
    call check_sum(a(1,1,2))
    a = reshape([(real(i), i = 1, 4)], [1, 2, 2])
    call check_values(a(1,1,2))
    allocate(b(2,3))
    b = reshape([(i, i = 1, 6)], [2, 3])
    call check_2d(b(1,2), [3, 4, 5, 6])
    call check_2d(b(2,1), [2, 3, 4, 5])
    call check_n(b(2,1), 2, [2, 3, 4, 5])
    call check_1d(b(2,1), [2, 3, 4, 5])
    print *, "ok"
contains
    subroutine check_sum(x)
        real, intent(in) :: x(1,2,1)
        if (abs(sum(x) - 2.0) > 1e-6) error stop 1
    end subroutine
    subroutine check_values(x)
        real, intent(in) :: x(1,2,1)
        if (abs(x(1,1,1) - 3.0) > 1e-6) error stop 2
        if (abs(x(1,2,1) - 4.0) > 1e-6) error stop 3
    end subroutine
    subroutine check_2d(y, expected)
        integer, intent(in) :: y(2,2), expected(4)
        if (any(y /= reshape(expected, [2, 2]))) error stop 4
    end subroutine
    subroutine check_n(y, n, expected)
        integer, intent(in) :: n
        integer, intent(in) :: y(n,n), expected(4)
        if (any(y /= reshape(expected, [2, 2]))) error stop 5
    end subroutine
    subroutine check_1d(y, expected)
        integer, intent(in) :: y(4), expected(4)
        if (any(y /= expected)) error stop 6
    end subroutine
end program
