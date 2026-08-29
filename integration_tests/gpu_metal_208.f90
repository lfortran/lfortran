module gpu_metal_208_mod
implicit none

type :: inner_t
    integer :: k_
end type

type :: op_t
    integer :: m_
    type(inner_t) :: in_
end type

contains

    pure function unit_vector(dir, length) result(u)
        integer, intent(in) :: dir, length
        real :: u(length)
        integer :: i
        do i = 1, length
            u(i) = 0.0
        end do
        u(dir) = real(length)
    end function

end module

program gpu_metal_208
use gpu_metal_208_mod
implicit none
type(op_t) :: op
real :: d(6,3)
integer :: col, i, j, n
real :: expected

op%m_ = 2
op%in_%k_ = 2
n = 1

! extent = op%m_ + 1 = 3
d = 0.0
do concurrent (col=1:3)
    d(1:3,col) = unit_vector(col, op%m_ + 1)
end do
do j = 1, 3
    do i = 1, 3
        expected = 0.0
        if (i == j) expected = 3.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop
    end do
end do

! extent = 2 * op%m_ = 4
d = 0.0
do concurrent (col=1:3)
    d(1:4,col) = unit_vector(col, 2 * op%m_)
end do
do j = 1, 3
    do i = 1, 4
        expected = 0.0
        if (i == j) expected = 4.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop
    end do
end do

! extent = op%m_ + n = 3
d = 0.0
do concurrent (col=1:3)
    d(1:3,col) = unit_vector(col, op%m_ + n)
end do
do j = 1, 3
    do i = 1, 3
        expected = 0.0
        if (i == j) expected = 3.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop
    end do
end do

! extent = op%in_%k_ + 1 = 3 (nested component)
d = 0.0
do concurrent (col=1:3)
    d(1:3,col) = unit_vector(col, op%in_%k_ + 1)
end do
do j = 1, 3
    do i = 1, 3
        expected = 0.0
        if (i == j) expected = 3.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop
    end do
end do

! extent = 2 * op%m_ + 1 = 5, larger than any of the above
d = 0.0
do concurrent (col=1:3)
    d(1:5,col) = unit_vector(col, 2 * op%m_ + 1)
end do
do j = 1, 3
    do i = 1, 5
        expected = 0.0
        if (i == j) expected = 5.0
        if (abs(d(i,j) - expected) > 1.0e-6) error stop
    end do
end do

print *, "ok"
end program
