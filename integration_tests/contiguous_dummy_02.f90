! A non-contiguous actual argument copied to a temporary for a call in the
! test of an IF, ELSE IF or DO WHILE, or in the bounds of a DO loop, is copied
! back right after the test is evaluated: the block sees the new values, and
! EXIT, CYCLE and RETURN in it do not skip the copy-back. Both a CONTIGUOUS
! assumed-shape dummy (#17133) and an explicit-shape dummy are copied.
module contiguous_dummy_02_m
implicit none
contains

integer function fc(x)
integer, contiguous :: x(:)
x = x + 1
fc = sum(x)
end function

integer function fe(x)
integer :: x(3)
x = x + 1
fe = sum(x)
end function

subroutine check(a, expected)
integer, intent(in) :: a(:), expected(:)
if (any(a /= expected)) then
    print *, a
    error stop
end if
end subroutine

subroutine ret_c(a)
integer, intent(inout) :: a(:)
if (fc(a(1::2)) > 0) return
a = 0
end subroutine

subroutine ret_e(a)
integer, intent(inout) :: a(:)
if (fe(a(1::2)) > 0) return
a = 0
end subroutine

end module

program contiguous_dummy_02
use contiguous_dummy_02_m
implicit none
integer, target :: a(6)
integer, pointer :: q(:)
integer :: i, n

! IF
a = [1, 2, 3, 4, 5, 6]
if (fc(a(1::2)) == 12) then
    call check(a, [2, 2, 4, 4, 6, 6])
else
    error stop
end if
call check(a, [2, 2, 4, 4, 6, 6])
a = [1, 2, 3, 4, 5, 6]
if (fe(a(2::2)) == 15) then
    call check(a, [1, 3, 3, 5, 5, 7])
else
    error stop
end if
call check(a, [1, 3, 3, 5, 5, 7])

! ELSE IF
a = [1, 2, 3, 4, 5, 6]
if (a(1) > 100) then
    error stop
else if (fc(a(2::2)) /= 15) then
    error stop
else if (fe(a(1::2)) == 12) then
    call check(a, [2, 3, 4, 5, 6, 7])
else
    error stop
end if
call check(a, [2, 3, 4, 5, 6, 7])

! EXIT and RETURN in the block of an IF
a = [1, 2, 3, 4, 5, 6]
n = 0
do i = 1, 10
    n = n + 1
    if (fc(a(1::2)) > 20) exit
end do
if (n /= 4) error stop
call check(a, [5, 2, 7, 4, 9, 6])
a = [1, 2, 3, 4, 5, 6]
n = 0
do i = 1, 10
    n = n + 1
    if (fe(a(1::2)) > 20) exit
end do
if (n /= 4) error stop
call check(a, [5, 2, 7, 4, 9, 6])
a = [1, 2, 3, 4, 5, 6]
call ret_c(a)
call check(a, [2, 2, 4, 4, 6, 6])
a = [1, 2, 3, 4, 5, 6]
call ret_e(a)
call check(a, [2, 2, 4, 4, 6, 6])

! DO WHILE, with CYCLE and EXIT in the body
a = [1, 2, 3, 4, 5, 6]
n = 0
do while (fc(a(1::2)) < 21)
    n = n + 1
    call check(a(1::2), [1, 3, 5] + n)
    if (mod(n, 2) == 1) cycle
    a(2) = a(2) + 1
end do
if (n /= 3) error stop
call check(a, [5, 3, 7, 4, 9, 6])
a = [1, 2, 3, 4, 5, 6]
n = 0
do while (fe(a(1::2)) < 21)
    n = n + 1
    call check(a(1::2), [1, 3, 5] + n)
    if (mod(n, 2) == 1) cycle
    a(2) = a(2) + 1
end do
if (n /= 3) error stop
call check(a, [5, 3, 7, 4, 9, 6])
a = [1, 2, 3, 4, 5, 6]
do while (fc(a(1::2)) < 100)
    if (a(1) >= 3) exit
end do
call check(a, [3, 2, 5, 4, 7, 6])
a = [1, 2, 3, 4, 5, 6]
do while (fe(a(1::2)) < 100)
    if (a(1) >= 3) exit
end do
call check(a, [3, 2, 5, 4, 7, 6])

! DO WHILE with a pointer actual
a = [1, 2, 3, 4, 5, 6]
q => a(1::2)
n = 0
do while (fc(q) < 21)
    n = n + 1
    call check(q, [1, 3, 5] + n)
end do
if (n /= 3) error stop
call check(a, [5, 2, 7, 4, 9, 6])
a = [1, 2, 3, 4, 5, 6]
n = 0
do while (fe(q) < 21)
    n = n + 1
    call check(q, [1, 3, 5] + n)
end do
if (n /= 3) error stop
call check(a, [5, 2, 7, 4, 9, 6])

! DO loop bounds
a = [1, 2, 3, 4, 5, 6]
n = 0
do i = 1, fc(a(1::2)) - 10
    n = n + 1
    call check(a, [2, 2, 4, 4, 6, 6])
end do
if (n /= 2) error stop
a = [1, 2, 3, 4, 5, 6]
n = 0
do i = 1, fe(a(1::2)) - 10
    n = n + 1
    call check(a, [2, 2, 4, 4, 6, 6])
end do
if (n /= 2) error stop

print *, "ok"
end program
