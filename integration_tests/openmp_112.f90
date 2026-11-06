program openmp_112
! sum() of an array component of a derived-type array element inside a
! parallel do, with the derived-type array declared in the program scope.
implicit none
type :: t
    integer :: tag
    real(8) :: y(2)
    real(8) :: total
end type
type(t) :: sw(0:3)
type(t) :: sw2(0:3)
integer :: i
do i = 0, 3
    sw(i)%tag = i
    sw(i)%y = i
    sw(i)%total = -1.0_8
    sw2(i)%tag = -i
    sw2(i)%y = 2*i
    sw2(i)%total = -1.0_8
end do
!$omp parallel do
do i = 0, 3
    sw(i)%total = sum(sw(i)%y)
    sw2(i)%total = sum(sw2(i)%y)
end do
!$omp end parallel do
do i = 0, 3
    if (abs(sw(i)%total - 2.0_8*i) > 1e-12) error stop
    if (abs(sw2(i)%total - 4.0_8*i) > 1e-12) error stop
    if (sw(i)%tag /= i) error stop
    if (sw2(i)%tag /= -i) error stop
end do
print *, sw%total
print *, sw2%total
end program
