module global_init_01_a
implicit none

type :: leaf
    integer :: h = 0
    character(len=3) :: tag = "bad"
end type

type(leaf) :: a_arr(3) = leaf(7, "aaa")

end module

module global_init_01_b
use global_init_01_a
implicit none

type(leaf) :: b_arr(2) = leaf(11, "bbb")

contains

subroutine check_a()
    integer :: i
    do i = 1, 3
        if (a_arr(i)%h /= 7) error stop 1
        if (a_arr(i)%tag /= "aaa") error stop 2
    end do
end subroutine

end module

program global_init_01
use global_init_01_b
implicit none
type(leaf) :: p_arr(2) = leaf(13, "ppp")
integer :: i

! A module variable an initializer could not lay out as static data is set
! before the first statement of the program runs.
call check_a()

do i = 1, 2
    if (b_arr(i)%h /= 11) error stop 3
    if (b_arr(i)%tag /= "bbb") error stop 4
end do

do i = 1, 2
    if (p_arr(i)%h /= 13) error stop 5
    if (p_arr(i)%tag /= "ppp") error stop 6
end do

! Each element is a copy, not an alias of one broadcast element.
a_arr(1)%h = 99
a_arr(1)%tag = "zzz"
if (a_arr(2)%h /= 7) error stop 7
if (a_arr(2)%tag /= "aaa") error stop 8
b_arr(1)%h = 98
if (b_arr(2)%h /= 11) error stop 9

print *, "ok"
end program
