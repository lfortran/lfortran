program pointer_section_06
    ! Pointer bounds-remapping onto rank-one data targets, which need not
    ! be contiguous, and onto simply contiguous data targets of higher rank
    implicit none
    integer, target :: a(10), b(4, 5)
    integer, allocatable, target :: c(:, :)
    integer, pointer :: p(:), q(:, :)
    integer :: i, j

    a = [(i, i = 1, 10)]
    do j = 1, 5
        do i = 1, 4
            b(i, j) = 10*i + j
        end do
    end do

    ! A strided section of rank one, in array element order
    q(1:2, 1:2) => a(1:9:2)
    if (q(1, 1) /= 1 .or. q(2, 1) /= 3 .or. q(1, 2) /= 5 .or. q(2, 2) /= 7) error stop 1
    p(0:2) => a(10:1:-3)
    if (p(0) /= 10 .or. p(1) /= 7 .or. p(2) /= 4) error stop 2

    ! A row of a matrix has rank one
    p(1:3) => b(2, 1:5:2)
    if (p(1) /= 21 .or. p(2) /= 23 .or. p(3) /= 25) error stop 3

    ! Simply contiguous data targets of rank two
    p(1:8) => b(:, 2:3)
    if (p(1) /= 12 .or. p(4) /= 42 .or. p(5) /= 13 .or. p(8) /= 43) error stop 4
    q(1:2, 1:2) => b(:, 4)
    if (q(1, 1) /= 14 .or. q(2, 2) /= 44) error stop 5
    p(1:20) => b
    if (p(2) /= 21 .or. p(20) /= 45) error stop 6
    allocate(c(3, 2))
    c = reshape([(i, i = 1, 6)], [3, 2])
    q(1:2, 1:3) => c
    if (q(2, 1) /= 2 .or. q(1, 2) /= 3 .or. q(2, 3) /= 6) error stop 7
    p(1:6) => contiguous_view()
    if (p(1) /= 11 .or. p(3) /= 31 .or. p(6) /= 22) error stop 8
    call remap_host()

    print *, "passed"

contains

    function contiguous_view() result(r)
        integer, pointer, contiguous :: r(:, :)
        r => b(:, 1:2)
    end function contiguous_view

    subroutine remap_host()
        integer, pointer :: s(:)
        s(1:4) => b(:, 5)
        if (s(1) /= 15 .or. s(4) /= 45) error stop 9
        s(1:20) => b
        if (s(5) /= 12) error stop 10
    end subroutine remap_host

end program pointer_section_06
