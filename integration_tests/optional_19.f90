! An absent optional intent(out) dummy of a type with default
! initialization is not initialized on entry (#16597). The caller passes a
! one-element stand-in for it, so initializing the whole explicit-shape
! dummy wrote far past the stand-in.
module optional_19_mod
implicit none
type :: t
    real :: x = 0
    integer :: k = 7
    character(len=5) :: c = "hello"
end type
type :: ta
    integer, allocatable :: q(:)
    integer :: z = 3
end type
contains
subroutine s(a, b)
    type(t), intent(out), optional :: a(10000000)
    type(t), intent(out), optional :: b(4)
    if (present(a)) error stop "a is absent"
    if (present(b)) b%x = 1
end subroutine
subroutine s2(a, b)
    type(t), intent(out), optional :: a(2000, 2000)
    type(t), intent(out), optional :: b(2, 3)
    if (present(a)) error stop "a is absent"
    if (present(b)) b%x = 2
end subroutine
subroutine s3(a, b)
    type(ta), intent(out), optional :: a(1000000)
    type(ta), intent(out), optional :: b(3)
    if (present(a)) error stop "a is absent"
    if (present(b)) b%z = 4
end subroutine
end module

program optional_19
use optional_19_mod
implicit none
type(t) :: b(4), b2(2, 3)
type(ta) :: c(3)
integer :: i
b%x = -1
b%k = -1
b%c = "xxxxx"
call s(b=b)
if (any(b%x /= 1)) error stop 1
if (any(b%k /= 7)) error stop 2
if (any(b%c /= "hello")) error stop 3

b2%x = -1
b2%k = -1
call s2(b=b2)
if (any(b2%x /= 2)) error stop 4
if (any(b2%k /= 7)) error stop 5

do i = 1, 3
    allocate(c(i)%q(2))
end do
c%z = -1
call s3(b=c)
if (any(c%z /= 4)) error stop 6
do i = 1, 3
    if (allocated(c(i)%q)) error stop 7
end do
print *, "ok"
end program
