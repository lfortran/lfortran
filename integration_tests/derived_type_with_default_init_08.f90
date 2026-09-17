module derived_type_with_default_init_08_mod
implicit none

type :: t
    integer :: h = 0
    real :: r = 0.0
end type

type :: u
    type(t) :: parts(2) = t(4, 2.5)
end type

type :: nested_leaf
    integer :: h
    character(len=3) :: tag
end type

type :: nested_mid
    type(nested_leaf) :: leaf
end type

type :: nested_outer
    type(nested_mid) :: mid
end type

type(t), parameter :: z = t(6, 1.5)
type(t), parameter :: pz(2, 2) = t(11, 6.5)
type(t) :: marr(3) = t(4, 2.5)
type(t) :: narr(2) = z
type(t) :: mat(2, 2) = t(7, 3.5)
type(t), save :: saved_arr(2) = t(12, 7.5)
type(u) :: gu

contains

subroutine check_t(x, h, r)
    type(t), intent(in) :: x
    integer, intent(in) :: h
    real, intent(in) :: r
    if (x%h /= h) error stop 1
    if (abs(x%r - r) > 1.e-6) error stop 2
end subroutine

subroutine check_procedure_local()
    type(t) :: larr(2) = t(9, 5.5)
    if (any(larr%h /= 9)) error stop 11
    if (any(abs(larr%r - 5.5) > 1.e-6)) error stop 12
end subroutine

subroutine check_nested_procedure_local()
    type(nested_outer) :: larr(3) = nested_outer(nested_mid(nested_leaf(21, "cat")))
    integer :: i

    do i = 1, 3
        if (larr(i)%mid%leaf%h /= 21) error stop 19
        if (larr(i)%mid%leaf%tag /= "cat") error stop 20
    end do

    larr(1)%mid%leaf%h = 42
    larr(1)%mid%leaf%tag = "dog"
    if (larr(2)%mid%leaf%h /= 21) error stop 21
    if (larr(2)%mid%leaf%tag /= "cat") error stop 22
    if (larr(3)%mid%leaf%h /= 21) error stop 23
    if (larr(3)%mid%leaf%tag /= "cat") error stop 24
end subroutine

end module

program derived_type_with_default_init_08
use derived_type_with_default_init_08_mod
implicit none
type(u) :: lu
type(t) :: parr(3) = t(8, 4.5)
type(t) :: qarr(2, 2) = pz
integer :: i, j

if (any(marr%h /= 4)) error stop 3
if (any(abs(marr%r - 2.5) > 1.e-6)) error stop 4
if (any(narr%h /= 6)) error stop 5
if (any(abs(narr%r - 1.5) > 1.e-6)) error stop 6
if (any(gu%parts%h /= 4)) error stop 7
if (any(abs(gu%parts%r - 2.5) > 1.e-6)) error stop 8
if (any(lu%parts%h /= 4)) error stop 9
if (any(abs(lu%parts%r - 2.5) > 1.e-6)) error stop 10
if (any(parr%h /= 8)) error stop 13
if (any(abs(parr%r - 4.5) > 1.e-6)) error stop 14
if (any(qarr%h /= 11)) error stop 15
if (any(abs(qarr%r - 6.5) > 1.e-6)) error stop 16
if (any(saved_arr%h /= 12)) error stop 17
if (any(abs(saved_arr%r - 7.5) > 1.e-6)) error stop 18

do j = 1, 2
    do i = 1, 2
        call check_t(mat(i, j), 7, 3.5)
    end do
end do

parr(1)%h = 99
parr(1)%r = -1.0
call check_t(parr(2), 8, 4.5)
call check_t(parr(3), 8, 4.5)

call check_procedure_local()
call check_nested_procedure_local()

print *, "ok"
end program
