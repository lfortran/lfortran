program pointer_section_07
    ! A pointer bounds-remapping with an upper bound below the lower bound
    ! gives a dimension of extent zero
    implicit none
    integer, target :: a(10), b(3, 4)
    integer, pointer :: p(:), q(:, :)
    integer :: n

    a = 1
    b = 2
    n = -5
    p(1:n) => a
    if (size(p) /= 0) error stop 1
    if (ubound(p, 1) /= 0) error stop 2
    p(3:n) => a(2:8:2)
    if (size(p) /= 0) error stop 3
    q(1:2, 4:n) => b
    if (size(q, 1) /= 2 .or. size(q, 2) /= 0 .or. size(q) /= 0) error stop 4
    print *, "passed"
end program pointer_section_07
