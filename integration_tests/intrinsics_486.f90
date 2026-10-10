program intrinsics_486
    ! dot_product of integer arrays held in descriptors (sections,
    ! allocatables, pointers) and of real arrays of different kinds (#13489)
    implicit none
    integer :: m(3, 3), v(3), r
    integer(8) :: m8(3, 3), r8
    integer, allocatable :: a(:), b(:)
    integer, target :: t(3), u(3)
    integer, pointer :: p(:), q(:)
    real(4), allocatable :: x(:)
    real(8), allocatable :: y(:)
    real(8) :: s

    m = 2
    r = dot_product(m(:,1), m(:,2))
    print *, r
    if (r /= 12) error stop

    m(:,1) = [1, 2, 3]
    m(:,3) = [4, 5, 6]
    if (dot_product(m(:,1), m(:,3)) /= 32) error stop
    if (dot_product(m(1,:), m(3,:)) /= 31) error stop

    v = 3
    if (dot_product(m(:,1), v) /= 18) error stop
    if (dot_product(v, m(:,3)) /= 45) error stop

    m8 = 2
    r8 = dot_product(m8(:,1), m8(:,2))
    if (r8 /= 12_8) error stop

    allocate(a(3), b(3))
    a = [1, 2, 3]
    b = [4, 5, 6]
    if (dot_product(a, b) /= 32) error stop

    t = [1, 2, 3]
    u = [7, 8, 9]
    p => t
    q => u
    if (dot_product(p, q) /= 50) error stop

    allocate(x(3), y(3))
    x = 2.0
    y = 3.0d0
    s = dot_product(x, y)
    if (abs(s - 18.0d0) > 1.0d-10) error stop
end program
