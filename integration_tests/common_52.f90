! Modules in separate files declare /common_52_blk/ with different layouts.
! The program uses their COMMON variables and also declares the block itself:
! all of them are associated by storage position.
program common_52
    use common_52_left, only: n, a, m, fill_matrix
    use common_52_right, only: set_values, v, fill_vector
    use common_52_swap, only: bump, sa => a, sn => n
    use common_52_relay, only: first, rest
    implicit none
    integer(4) :: p(3)
    common /common_52_blk/ p
    call set_values()
    if (n /= 7) error stop
    if (any(a /= [11, 13])) error stop
    if (sa /= 7) error stop
    if (any(sn /= [11, 13])) error stop
    if (first /= 7) error stop
    if (any(rest /= [11, 13])) error stop
    if (any(p /= [7, 11, 13])) error stop
    call bump()
    if (any(p /= [8, 22, 26])) error stop
    n = 1
    rest(2) = 3
    print *, p
    if (any(p /= [1, 22, 3])) error stop
    if (sa /= 1) error stop
    if (any(sn /= [22, 3])) error stop
    ! A vector written through one module is a matrix in column major order
    ! through the other, and the other way round.
    call fill_vector()
    if (any(m(:, 1) /= [1, 2])) error stop
    if (any(m(:, 2) /= [3, 4])) error stop
    if (any(m(:, 3) /= [5, 6])) error stop
    call fill_matrix()
    print *, v
    if (any(v /= [11, 21, 12, 22, 13, 23])) error stop
    ! The same storage through array descriptors: sections of either view and
    ! assumed-shape dummy arguments, read and written.
    if (any(m(2, :) /= v(2::2))) error stop
    call check_matrix(m)
    call check_row(m(1, :))
    call check_row(v(1::2))
    call add_to(m(2, :), 100)
    if (any(v /= [11, 121, 12, 122, 13, 123])) error stop
    call add_to(v(1::2), 1000)
    if (any(m(1, :) /= [1011, 1012, 1013])) error stop
contains
    subroutine check_matrix(x)
        integer(4), intent(in) :: x(:, :)
        if (size(x, 1) /= 2 .or. size(x, 2) /= 3) error stop
        if (any(x(:, 3) /= [13, 23])) error stop
        if (any(x(2, :) /= [21, 22, 23])) error stop
    end subroutine check_matrix

    subroutine check_row(r)
        integer(4), intent(in) :: r(:)
        if (any(r /= [11, 12, 13])) error stop
    end subroutine check_row

    subroutine add_to(r, k)
        integer(4), intent(inout) :: r(:)
        integer(4), intent(in) :: k
        r = r + k
    end subroutine add_to
end program common_52
