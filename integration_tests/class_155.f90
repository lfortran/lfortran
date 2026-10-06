module class_155_mod
implicit none
type :: t
    integer :: y
end type
type, extends(t) :: t2
    integer :: z(64)
end type
contains
subroutine check(a)
    class(t), intent(in) :: a(:)
    integer :: k(2)
    k = a(3:4)%y
    if (any(k /= [3, 4])) error stop
end subroutine
end module

program class_155
use class_155_mod
implicit none
class(t), allocatable :: cw(:)
integer, allocatable :: after(:)
integer :: i, k(2), m(3)
allocate(t2 :: cw(4))
select type (cw)
type is (t2)
    do i = 1, 4
        cw(i)%y = i
        cw(i)%z = 10*i
    end do
end select

k = cw(1:2)%y
if (any(k /= [1, 2])) error stop
k = cw(2:4:2)%y
if (any(k /= [2, 4])) error stop
k = cw([1, 3])%y
if (any(k /= [1, 3])) error stop
if (sum(cw(1:2)%y) /= 3) error stop
call check(cw)

cw(2:4)%y = cw(1:3)%y
m = cw(2:4)%y
if (any(m /= [1, 2, 3])) error stop

allocate(after(1000))
after = 7
if (any(after /= 7)) error stop
select type (cw)
type is (t2)
    do i = 1, 4
        if (any(cw(i)%z /= 10*i)) error stop
    end do
class default
    error stop
end select
print *, k, m
end program
