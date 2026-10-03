module global_init_02_m
implicit none

type :: box
    integer :: h = 0
    character(len=3) :: tag = "bad"
end type

contains

! A local whose declaration initializer has to be stored by executable code
! still has the save attribute Fortran gives every initialized local, so it
! is initialized once and keeps what the previous call left in it.
subroutine bump(expected)
    integer, intent(in) :: expected
    type(box), save :: arr(3) = box(7, "aaa")
    integer :: i

    if (arr(1)%h /= expected) error stop 1
    do i = 2, 3
        if (arr(i)%h /= 7) error stop 2
    end do
    do i = 1, 3
        if (arr(i)%tag /= "aaa") error stop 3
    end do

    ! Each element is a copy, not an alias of one broadcast element.
    arr(1)%h = arr(1)%h + 1
end subroutine

! The same without an explicit save attribute: an initialized local is saved
! implicitly, so this behaves exactly like `bump` above.
subroutine bump_implicit(expected)
    integer, intent(in) :: expected
    type(box) :: arr(2) = box(20, "iii")

    if (arr(1)%h /= expected) error stop 4
    if (arr(2)%h /= 20) error stop 5
    if (arr(1)%tag /= "iii") error stop 6
    arr(1)%h = arr(1)%h + 1
end subroutine

end module

program global_init_02
use global_init_02_m
implicit none

call bump(7)
call bump(8)
call bump(9)

call bump_implicit(20)
call bump_implicit(21)

print *, "ok"
end program
