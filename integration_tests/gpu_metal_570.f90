! A procedure called in a `do concurrent` loop reads entities of its module
! that the caller cannot see, or sees under another name, or whose name the
! caller uses for an entity of its own.
module gpu_metal_570_m2
implicit none
real :: s = 3
end module

module gpu_metal_570_m
use gpu_metal_570_m2, only: s
implicit none
real :: w = 2
integer, parameter :: n = 3
contains
pure real function f1(i)
    integer, intent(in) :: i
    f1 = i + w
end function

pure real function f2(i)
    integer, intent(in) :: i
    f2 = i * n
end function

pure real function f3(i)
    integer, intent(in) :: i
    f3 = s * i
end function
end module

module gpu_metal_570_c
implicit none
contains
subroutine only_f1(o)
    use gpu_metal_570_m, only: f1
    real, intent(out) :: o(10)
    integer :: i
    do concurrent (i = 1:10)
        o(i) = f1(i)
    end do
end subroutine
end module

program gpu_metal_570
use gpu_metal_570_m, only: f1, f2, f3, mw => w
use gpu_metal_570_c, only: only_f1
implicit none
real :: o(10), w
integer :: i, n
w = 1000
n = 1

call only_f1(o)
do i = 1, 10
    if (abs(o(i) - (i + 2)) > 1e-5) error stop 1
end do

mw = 50
do concurrent (i = 1:10)
    o(i) = f1(i)
end do
do i = 1, 10
    if (abs(o(i) - (i + 50)) > 1e-5) error stop 2
end do

do concurrent (i = 1:10)
    o(i) = f2(i) + n
end do
do i = 1, 10
    if (abs(o(i) - (3 * i + 1)) > 1e-5) error stop 3
end do

do concurrent (i = 1:10)
    o(i) = f3(i)
end do
do i = 1, 10
    if (abs(o(i) - 3 * i) > 1e-5) error stop 4
end do

do concurrent (i = 1:10)
    o(i) = f1(i) + mw + w
end do
do i = 1, 10
    if (abs(o(i) - (i + 1100)) > 1e-5) error stop 5
end do

call contained()
print *, "ok"

contains

subroutine contained()
    integer :: j
    do concurrent (j = 1:10)
        o(j) = f1(j) + w
    end do
    do j = 1, 10
        if (abs(o(j) - (j + 1050)) > 1e-5) error stop 6
    end do
end subroutine
end program
