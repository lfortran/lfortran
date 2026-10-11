program array_section_37
    ! Sections that select no elements, or only elements within the
    ! bounds, are valid whatever their triplet bounds are.
    implicit none
    real, allocatable :: z(:), a(:)
    real :: f(0), b(2, 3)
    real, pointer :: p(:)
    real, target :: t(3)
    integer :: n

    allocate(z(0), a(3))
    a = [1.0, 2.0, 3.0]
    b = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
    t = a
    n = 4

    if (size(z(1:0)) /= 0) error stop
    if (size(z(5:4)) /= 0) error stop
    if (size(f(1:0)) /= 0) error stop
    if (size(a(1:0)) /= 0) error stop
    if (size(a(n:n-1)) /= 0) error stop
    if (size(a(1:3:-1)) /= 0) error stop

    if (size(a(1:4:5)) /= 1) error stop
    if (any(a(1:n:5) /= [1.0])) error stop
    if (any(a(3:1:-1) /= [3.0, 2.0, 1.0])) error stop
    if (any(a(1:n:2) /= [1.0, 3.0])) error stop

    if (any(b(2, 1:3) /= [2.0, 4.0, 6.0])) error stop
    if (any(b(1:2, 3) /= [5.0, 6.0])) error stop
    if (size(b(2, n:3)) /= 0) error stop

    call check(a(2:3), 5.0)
    p => t(n-1:1:-2)
    if (size(p) /= 2) error stop
    if (any(p /= [3.0, 1.0])) error stop
    print *, "ok"

contains

    subroutine check(x, expected)
        real, intent(in) :: x(:)
        real, intent(in) :: expected
        if (sum(x) /= expected) error stop
    end subroutine

end program
