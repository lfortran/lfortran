module class_154_mod
implicit none
type :: t
    integer :: y
end type
type, extends(t) :: t2
    integer :: z
end type
type :: box
    class(t), allocatable :: a(:)
end type
contains
subroutine check(x, expected)
    class(t), intent(in) :: x(:)
    integer, intent(in) :: expected(:)
    integer :: i
    if (size(x) /= size(expected)) error stop
    do i = 1, size(x)
        if (x(i)%y /= expected(i)) error stop
    end do
end subroutine
subroutine check_t2(x)
    class(t), intent(in) :: x(:)
    select type (x)
    type is (t2)
        if (x(1)%z /= 20) error stop
        if (x(2)%z /= 40) error stop
    class default
        error stop
    end select
end subroutine
subroutine check_any(x, expected)
    class(*), intent(in) :: x(:)
    integer, intent(in) :: expected(:)
    if (size(x) /= size(expected)) error stop
    select type (x)
    type is (integer)
        if (any(x /= expected)) error stop
    type is (t)
        if (any(x%y /= expected)) error stop
    class default
        error stop
    end select
end subroutine
subroutine bump(x)
    class(t), intent(inout) :: x(:)
    x%y = x%y + 100
end subroutine
end module

program class_154
use class_154_mod
implicit none
class(t), allocatable, target :: cw(:)
class(t), allocatable :: c2(:,:)
class(t), pointer :: p(:)
class(*), allocatable, target :: u(:)
class(*), pointer :: up(:)
type(box) :: b
integer :: k(2), i, j
allocate(cw(4))
do i = 1, 4
    cw(i)%y = i
end do
print *, cw(2:3)%y
k = cw(2:3)%y
if (any(k /= [2, 3])) error stop
call check(cw(2:3), [2, 3])
call check(cw(1:4:2), [1, 3])
call check(cw(4:1:-2), [4, 2])
k = cw(2:4:2)%y
if (any(k /= [2, 4])) error stop
call bump(cw(2:4:2))
if (any(cw%y /= [1, 102, 3, 104])) error stop
deallocate(cw)

allocate(t2 :: cw(4))
select type (cw)
type is (t2)
    do i = 1, 4
        cw(i)%y = i
        cw(i)%z = 10*i
    end do
end select
call check(cw(2:3), [2, 3])
call check(cw(2:4:2), [2, 4])
call check_t2(cw(2:4:2))
p => cw(2:3)
call check(p, [2, 3])
p => cw(4:1:-2)
call check(p, [4, 2])

allocate(c2(3,3))
do j = 1, 3
    do i = 1, 3
        c2(i,j)%y = 10*i + j
    end do
end do
call check(c2(2, 2:3), [22, 23])
call check(c2(2:3, 3), [23, 33])

allocate(b%a(3))
do i = 1, 3
    b%a(i)%y = 5*i
end do
k = b%a(2:3)%y
if (any(k /= [10, 15])) error stop
call check(b%a(2:3), [10, 15])

allocate(u, source=[1, 2, 3, 4])
call check_any(u(2:3), [2, 3])
call check_any(u(4:1:-2), [4, 2])
up => u(2:4:2)
select type (up)
type is (integer)
    print *, up
    if (any(up /= [2, 4])) error stop
class default
    error stop
end select
call check_any(up, [2, 4])
deallocate(u)

allocate(t :: u(4))
select type (u)
type is (t)
    do i = 1, 4
        u(i)%y = 10*i
    end do
end select
call check_any(u(2:3), [20, 30])
up => u(3:4)
call check_any(up, [30, 40])
print *, "ok"
end program
