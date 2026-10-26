program array_section_41
    ! Contiguous sections of fixed-size arrays passed to explicit-shape
    ! dummies are passed as they are, without a run-time contiguity check.
    implicit none
    integer :: b(6), i
    real :: r(5)
    do i = 1, 6
        b(i) = i
    end do
    r = [1.0, 2.0, 3.0, 4.0, 5.0]

    call bump(b(2:4), 3)
    print *, b
    if (any(b /= [1, 12, 13, 14, 5, 6])) error stop 1

    call bump(b(5:), 2)
    print *, b
    if (any(b /= [1, 12, 13, 14, 15, 16])) error stop 2

    call bump(b(1:6:1), 6)
    print *, b
    if (any(b /= [11, 22, 23, 24, 25, 26])) error stop 3

    call scale(r(2:3), 2)
    print *, r
    if (any(abs(r - [1.0, 4.0, 6.0, 4.0, 5.0]) > 1e-6)) error stop 4
contains
    subroutine bump(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = x(i) + 10
        end do
    end subroutine

    subroutine scale(x, n)
        integer, intent(in) :: n
        real, intent(inout) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = 2*x(i)
        end do
    end subroutine
end program
