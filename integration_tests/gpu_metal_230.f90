! An array-valued ASSOCIATE inside a `do concurrent` whose selector is a
! DESCRIPTOR array (an allocatable local or an assumed-shape dummy) has no
! static shape: the temporary the frontend materialises for the selector
! carries a null `dimension_t`. The element loop synthesized for it must
! therefore take its bounds at run time, and the loop cannot be offloaded
! because a Metal kernel cannot declare a variable-length local array.
program gpu_metal_230
implicit none
real, allocatable :: x(:)
real, allocatable :: y(:,:)
real :: g(3), h(2,3), p(3), q(2,3)
integer :: i, j

allocate(x(3))
x = [1.0, 2.0, 3.0]
allocate(y(2,3))
do j = 1, 3
    do i = 1, 2
        y(i,j) = real(10*i + j)
    end do
end do

! rank 1, allocatable operand
g = 0.0
do concurrent (j = 1:3)
    associate (r => x*2.0)
        g(j) = r(j)
    end associate
end do

! rank 2, allocatable operand
h = 0.0
do concurrent (j = 1:3)
    associate (r => y + 1.0)
        h(1,j) = r(1,j)
        h(2,j) = r(2,j)
    end associate
end do

call rank1_dummy(x, p)
call rank2_dummy(y, q)

do j = 1, 3
    if (abs(g(j) - 2.0*x(j)) > 1.0e-5) error stop "g"
    if (abs(p(j) - (x(j) + 5.0)) > 1.0e-5) error stop "p"
    do i = 1, 2
        if (abs(h(i,j) - (y(i,j) + 1.0)) > 1.0e-5) error stop "h"
        if (abs(q(i,j) - 3.0*y(i,j)) > 1.0e-5) error stop "q"
    end do
end do

print *, g(1), h(1,1), p(1), q(1,1)
print *, "ok"

contains

    ! rank 1, assumed-shape dummy operand
    subroutine rank1_dummy(a, res)
    real, intent(in) :: a(:)
    real, intent(out) :: res(3)
    integer :: k
    res = 0.0
    do concurrent (k = 1:3)
        associate (r => a + 5.0)
            res(k) = r(k)
        end associate
    end do
    end subroutine

    ! rank 2, assumed-shape dummy operand
    subroutine rank2_dummy(a, res)
    real, intent(in) :: a(:,:)
    real, intent(out) :: res(2,3)
    integer :: k
    res = 0.0
    do concurrent (k = 1:3)
        associate (r => 3.0*a)
            res(1,k) = r(1,k)
            res(2,k) = r(2,k)
        end associate
    end do
    end subroutine

end program
