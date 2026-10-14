! Elementwise array assignments whose loops index every operand within its
! bounds: sections assigned from array constants, fixed-size and
! non-default lower bound operands, multi-dimensional, allocatable, pointer
! and component operands.
module arrays_op_32_mod
    implicit none
    real :: tab(8, 3)
contains
    subroutine init()
        tab(:, 1) = [1., 2., 3., 4., 5., 6., 7., 8.]
        tab(:, 2) = [2., 4., 6., 8., 10., 12., 14., 16.]
        tab(:, 3) = [3., 6., 9., 12., 15., 18., 21., 24.]
    end subroutine
end module

program arrays_op_32
    use arrays_op_32_mod, only: tab, init
    implicit none
    type :: holder
        integer :: v(4)
    end type
    integer :: i, j
    integer :: x(4), y(4), z(4)
    integer :: lb(-2:1), m(3, 2), m2(3, 2)
    integer, allocatable :: al(:), al2(:)
    integer, target :: t(6)
    integer, pointer :: p(:)
    type(holder) :: h

    call init()
    do j = 1, 3
        do i = 1, 8
            if (abs(tab(i, j) - real(i*j)) > 1e-6) error stop
        end do
    end do

    y = [1, 2, 3, 4]
    z = [10, 20, 30, 40]
    x = y + z
    if (any(x /= [11, 22, 33, 44])) error stop

    lb = x
    if (lb(-2) /= 11 .or. lb(1) /= 44) error stop
    x = lb * 2
    if (any(x /= [22, 44, 66, 88])) error stop

    m = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    m2 = m + 1
    if (any(m2(:, 2) /= [5, 6, 7])) error stop
    m2(2, :) = [0, 0]
    if (m2(2, 1) /= 0 .or. m2(2, 2) /= 0 .or. m2(3, 2) /= 7) error stop

    allocate(al(4), al2(4))
    al = y
    al2 = al + z
    if (any(al2 /= [11, 22, 33, 44])) error stop

    t = 0
    p => t(2:5)
    p = y
    if (any(t /= [0, 1, 2, 3, 4, 0])) error stop
    p => t(6:1:-2)
    p = [7, 8, 9]
    if (any(t /= [0, 9, 2, 8, 4, 7])) error stop

    h%v = z
    h%v = h%v + y
    if (any(h%v /= [11, 22, 33, 44])) error stop

    x = x(4:1:-1)
    if (any(x /= [88, 66, 44, 22])) error stop
    print *, "ok"
end program
