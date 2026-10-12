module class_161_mod
implicit none
type :: item_t
    integer :: x = 0
end type
type, extends(item_t) :: ext_t
    integer :: y = 0
end type
type :: holder_t
    class(item_t), allocatable :: item
    class(item_t), pointer :: pitem => null()
end type
type :: outer_t
    type(holder_t) :: h
end type
contains
subroutine test(holder)
    type(holder_t) :: holder
    call consume(holder%item)
    call consume(holder%pitem)
end subroutine
subroutine consume(item)
    type(item_t) :: item
    item%x = item%x + 1
end subroutine
subroutine check_in(item, expected)
    type(item_t), intent(in) :: item
    integer, intent(in) :: expected
    if (item%x /= expected) error stop
end subroutine
subroutine add_ten(item)
    type(item_t), intent(inout) :: item
    item%x = item%x + 10
end subroutine
integer function get_x(item)
    type(item_t), intent(in) :: item
    get_x = item%x
end function
end module

program class_161
use class_161_mod
implicit none
type(holder_t) :: h
type(outer_t) :: o
allocate(ext_t :: h%item)
allocate(ext_t :: h%pitem)
h%item%x = 5
h%pitem%x = 1
select type (it => h%item)
type is (ext_t)
    it%y = 42
end select

call test(h)
if (h%item%x /= 6) error stop
if (h%pitem%x /= 2) error stop

call check_in(h%item, 6)
call check_in(h%pitem, 2)
if (get_x(h%item) /= 6) error stop
if (get_x(h%pitem) /= 2) error stop

call add_ten(h%item)
call add_ten(h%pitem)
if (h%item%x /= 16) error stop
if (h%pitem%x /= 12) error stop

select type (it => h%item)
type is (ext_t)
    if (it%y /= 42) error stop
class default
    error stop
end select

allocate(item_t :: o%h%item)
o%h%item%x = 3
call consume(o%h%item)
call check_in(o%h%item, 4)

deallocate(h%pitem)
print *, h%item%x, o%h%item%x
end program
