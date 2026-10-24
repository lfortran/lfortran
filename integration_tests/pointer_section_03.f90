program pointer_section_03
    ! Pointer bounds-spec and bounds-remapping onto sections of arrays
    ! without a descriptor (#11847, #12177)
    implicit none
    integer, target :: values(4), tg(8), a(10), b(4, 5), x(10, 10)
    integer, pointer :: p(:), q(:, :)
    integer :: i, j

    values = [10, 20, 30, 40]
    tg = [1, 2, 3, 4, 5, 6, 7, 8]
    a = [(i, i = 1, 10)]
    do j = 1, 5
        do i = 1, 4
            b(i, j) = 10*i + j
        end do
    end do
    do j = 1, 10
        do i = 1, 10
            x(i, j) = 100*i + j
        end do
    end do

    ! Contiguous section
    p(5:) => values(2:4)
    if (lbound(p, 1) /= 5) error stop 1
    if (ubound(p, 1) /= 7) error stop 2
    if (size(p) /= 3) error stop 3
    if (p(5) /= 20 .or. p(6) /= 30 .or. p(7) /= 40) error stop 4

    ! Strided section
    p(0:) => tg(1:8:3)
    if (lbound(p, 1) /= 0) error stop 5
    if (ubound(p, 1) /= 2) error stop 6
    if (p(0) /= 1 .or. p(1) /= 4 .or. p(2) /= 7) error stop 7

    ! Strided rank-reduced section
    p(4:) => x(4:7:3, 5)
    call check(p, 4, 5)
    if (p(4) /= 405 .or. p(5) /= 705) error stop 8

    ! Rank-reduced section along the second dimension
    p(-1:) => b(2, 2:5:2)
    if (lbound(p, 1) /= -1 .or. ubound(p, 1) /= 0) error stop 9
    if (p(-1) /= 22 .or. p(0) /= 24) error stop 10

    ! Two-dimensional strided section
    q(0:, 0:) => b(1:4:2, 2:5:2)
    if (lbound(q, 1) /= 0 .or. ubound(q, 1) /= 1) error stop 11
    if (lbound(q, 2) /= 0 .or. ubound(q, 2) /= 1) error stop 12
    if (q(0, 0) /= 12 .or. q(1, 0) /= 32) error stop 13
    if (q(0, 1) /= 14 .or. q(1, 1) /= 34) error stop 14

    ! Whole array
    p(-2:) => a
    if (lbound(p, 1) /= -2 .or. ubound(p, 1) /= 7) error stop 15
    if (p(-2) /= 1 .or. p(7) /= 10) error stop 16

    ! Bounds-remapping onto a strided rank-one section
    p(2:3) => a(2:10:4)
    if (lbound(p, 1) /= 2 .or. ubound(p, 1) /= 3) error stop 17
    if (p(2) /= 2 .or. p(3) /= 6) error stop 18

    ! Bounds-remapping of a rank-one section to rank two
    q(0:1, 3:5) => a(3:8)
    if (lbound(q, 1) /= 0 .or. ubound(q, 1) /= 1) error stop 19
    if (lbound(q, 2) /= 3 .or. ubound(q, 2) /= 5) error stop 20
    if (q(0, 3) /= 3 .or. q(1, 3) /= 4 .or. q(0, 4) /= 5) error stop 21
    if (q(1, 5) /= 8) error stop 22

    ! Writing through the pointer
    p(0:) => tg(2:8:2)
    p(1) = 99
    if (tg(4) /= 99) error stop 23

    print *, "passed"

contains

    subroutine check(c, lb, ub)
        integer, pointer, intent(in) :: c(:)
        integer, intent(in) :: lb, ub
        if (lbound(c, 1) /= lb) error stop 30
        if (ubound(c, 1) /= ub) error stop 31
        if (size(c) /= ub - lb + 1) error stop 32
    end subroutine check

end program pointer_section_03
