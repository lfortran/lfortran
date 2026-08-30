! A `matmul` that the statement-level matmul lowering cannot see -- one
! nested inside an array constructor or under a unary minus -- is hoisted
! into its own temporary first.  When the operands are deferred-shape the
! result shape is only known at run time, so the temporary is sized from
! the operands' run-time extents and lives in the loop body's BLOCK, where
! each thread gets its own workspace slice.  Without that, the matmul
! survives into the shader as a call to the host runtime helper, which
! does not exist on the device.
module gpu_metal_246_mod
implicit none

contains

    ! matmul(rank 2, rank 1) inside an array constructor.
    pure function edge_padded(a, v) result(r)
    real, allocatable, intent(in) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    real, allocatable :: r(:)
    r = [0.0, matmul(a, v), 0.0]
    end function

    ! matmul(rank 1, rank 2) inside an array constructor.
    pure function row_padded(v, b) result(r)
    real, allocatable, intent(in) :: v(:)
    real, allocatable, intent(in) :: b(:,:)
    real, allocatable :: r(:)
    r = [matmul(v, b), 0.0]
    end function

    ! Two matmuls in one array constructor.
    pure function two_products(a, v) result(r)
    real, allocatable, intent(in) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    real, allocatable :: r(:)
    r = [matmul(a, v), matmul(a, v)]
    end function

    ! A matmul whose own argument is another matmul.
    pure function nested(a, v) result(r)
    real, allocatable, intent(in) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    real, allocatable :: r(:)
    r = [matmul(a, matmul(a, v)), 1.0]
    end function

    subroutine run_edge(o, a, v, nn)
    real, intent(out) :: o(:,:)
    real, allocatable, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = edge_padded(a, v)
    end do
    end subroutine

    subroutine run_row(o, v, b, nn)
    real, intent(out) :: o(:,:)
    real, allocatable, intent(in) :: v(:), b(:,:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = row_padded(v, b)
    end do
    end subroutine

    subroutine run_two(o, a, v, nn)
    real, intent(out) :: o(:,:)
    real, allocatable, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = two_products(a, v)
    end do
    end subroutine

    subroutine run_nested(o, a, v, nn)
    real, intent(out) :: o(:,:)
    real, allocatable, intent(in) :: a(:,:), v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        o(:, jj) = nested(a, v)
    end do
    end subroutine

end module

program gpu_metal_246
use gpu_metal_246_mod
implicit none
integer, parameter :: n = 5
real, allocatable :: a(:,:), b(:,:), v(:)
real :: o4(4, n), o3(3, n)
integer :: j

allocate(a(2, 2), b(2, 2), v(2))
! a = [1 3; 2 4] in column-major order
a = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
b = reshape([5.0, 6.0, 7.0, 8.0], [2, 2])
v = [1.0, 1.0]

! matmul(a, v) == [4, 6]
o4 = -1
call run_edge(o4, a, v, n)
do j = 1, n
    if (abs(o4(1, j) - 0.0) > 1.0e-4) error stop "edge1"
    if (abs(o4(2, j) - 4.0) > 1.0e-4) error stop "edge2"
    if (abs(o4(3, j) - 6.0) > 1.0e-4) error stop "edge3"
    if (abs(o4(4, j) - 0.0) > 1.0e-4) error stop "edge4"
end do

! matmul(v, b) == [11, 15]
o3 = -1
call run_row(o3, v, b, n)
do j = 1, n
    if (abs(o3(1, j) - 11.0) > 1.0e-4) error stop "row1"
    if (abs(o3(2, j) - 15.0) > 1.0e-4) error stop "row2"
    if (abs(o3(3, j) - 0.0) > 1.0e-4) error stop "row3"
end do

o4 = -1
call run_two(o4, a, v, n)
do j = 1, n
    if (abs(o4(1, j) - 4.0) > 1.0e-4) error stop "two1"
    if (abs(o4(2, j) - 6.0) > 1.0e-4) error stop "two2"
    if (abs(o4(3, j) - 4.0) > 1.0e-4) error stop "two3"
    if (abs(o4(4, j) - 6.0) > 1.0e-4) error stop "two4"
end do

! matmul(a, matmul(a, v)) == matmul(a, [4, 6]) == [4 + 18, 8 + 24] == [22, 32]
o3 = -1
call run_nested(o3, a, v, n)
do j = 1, n
    if (abs(o3(1, j) - 22.0) > 1.0e-4) error stop "nested1"
    if (abs(o3(2, j) - 32.0) > 1.0e-4) error stop "nested2"
    if (abs(o3(3, j) - 1.0) > 1.0e-4) error stop "nested3"
end do

print *, "ok"
end program
