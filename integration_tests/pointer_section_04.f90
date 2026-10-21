program pointer_section_04
    ! Pointer bounds-spec and bounds-remapping onto arrays with a
    ! descriptor keep the strides of a non-contiguous data target
    implicit none
    integer, target :: a(10)
    integer, allocatable, target :: b(:, :)
    integer, pointer :: p(:), r(:), q(:, :), s(:, :)
    integer :: i, j, ncalls

    a = [(i, i = 1, 10)]
    allocate(b(4, 5))
    do j = 1, 5
        do i = 1, 4
            b(i, j) = 10*i + j
        end do
    end do

    ! Bounds-spec onto a non-contiguous pointer
    r => a(1:10:2)
    p(3:) => r
    if (lbound(p, 1) /= 3 .or. ubound(p, 1) /= 7) error stop 1
    if (p(3) /= 1 .or. p(4) /= 3 .or. p(7) /= 9) error stop 2

    ! Bounds-remapping onto a non-contiguous pointer of rank one
    p(1:3) => r
    if (p(1) /= 1 .or. p(2) /= 3 .or. p(3) /= 5) error stop 3
    q(1:2, 1:2) => r
    if (q(1, 1) /= 1 .or. q(2, 1) /= 3 .or. q(1, 2) /= 5 .or. q(2, 2) /= 7) error stop 4

    ! Bounds-spec onto a section of an allocatable array
    q(0:, 0:) => b(1:4:2, 2:5:2)
    if (lbound(q, 1) /= 0 .or. ubound(q, 1) /= 1) error stop 5
    if (lbound(q, 2) /= 0 .or. ubound(q, 2) /= 1) error stop 6
    if (q(0, 0) /= 12 .or. q(1, 0) /= 32) error stop 7
    if (q(0, 1) /= 14 .or. q(1, 1) /= 34) error stop 8

    ! Bounds-spec onto a non-contiguous pointer of rank two
    s => b(1:4:2, 2:5:2)
    q(-1:, 5:) => s
    if (lbound(q, 1) /= -1 .or. ubound(q, 1) /= 0) error stop 9
    if (lbound(q, 2) /= 5 .or. ubound(q, 2) /= 6) error stop 10
    if (q(-1, 5) /= 12 .or. q(0, 5) /= 32) error stop 11
    if (q(-1, 6) /= 14 .or. q(0, 6) /= 34) error stop 12

    ! Bounds-spec onto a whole allocatable array
    q(2:, 3:) => b
    if (lbound(q, 1) /= 2 .or. ubound(q, 1) /= 5) error stop 13
    if (lbound(q, 2) /= 3 .or. ubound(q, 2) /= 7) error stop 14
    if (q(2, 3) /= 11 .or. q(5, 7) /= 45) error stop 15

    ! Bounds-remapping of the same rank to another shape
    q(1:2, 1:10) => b
    if (q(1, 1) /= 11 .or. q(2, 1) /= 21) error stop 16
    if (q(1, 2) /= 31 .or. q(2, 2) /= 41) error stop 17
    if (q(1, 3) /= 12 .or. q(2, 10) /= 45) error stop 18

    ! Bounds-spec onto a pointer function result, evaluated once
    ncalls = 0
    p(0:) => every_third()
    if (ncalls /= 1) error stop 19
    if (lbound(p, 1) /= 0 .or. ubound(p, 1) /= 2) error stop 20
    if (p(0) /= 2 .or. p(1) /= 5 .or. p(2) /= 8) error stop 21

    ! The subscripts of a bounds-spec data target are evaluated once
    ncalls = 0
    p(1:) => a(g(2):g(6))
    if (ncalls /= 2) error stop 22
    if (lbound(p, 1) /= 1 .or. ubound(p, 1) /= 5 .or. p(1) /= 2) error stop 23
    ncalls = 0
    p(1:) => a(g(2):)
    if (ncalls /= 1) error stop 24
    if (ubound(p, 1) /= 9 .or. p(9) /= 10) error stop 25
    ncalls = 0
    q(1:, 1:) => b(:, g(1):g(3))
    if (ncalls /= 2) error stop 26
    if (ubound(q, 1) /= 4 .or. ubound(q, 2) /= 3) error stop 27
    if (q(4, 3) /= 43) error stop 28
    ncalls = 0
    p(-1:) => b(3, g(2):g(5):g(2))
    if (ncalls /= 3) error stop 29
    if (lbound(p, 1) /= -1 .or. ubound(p, 1) /= 0) error stop 30
    if (p(-1) /= 32 .or. p(0) /= 34) error stop 31

    print *, "passed"

contains

    function every_third() result(r)
        integer, pointer :: r(:)
        ncalls = ncalls + 1
        r => a(2:10:3)
    end function every_third

    integer function g(i)
        integer, intent(in) :: i
        ncalls = ncalls + 1
        g = i
    end function g

end program pointer_section_04
