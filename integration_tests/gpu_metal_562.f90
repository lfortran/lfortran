! An exit or a cycle in an offloaded do concurrent: of a loop inside the
! body, of a named loop from a loop inside it, and of the do concurrent
! itself, which ends only that iteration; also in a routine the body
! calls.
module gpu_metal_562_m
implicit none
contains
pure integer function odd_sum(n) result(r)
    integer, intent(in) :: n
    integer :: k
    r = 0
    do k = 1, 10
        if (k > n) exit
        if (mod(k, 2) == 0) cycle
        r = r + k
    end do
end function
end module

program gpu_metal_562
use gpu_metal_562_m
implicit none
integer, parameter :: m = 256
real :: x(3), xm(m), y(4, m)
integer :: s(3), u(3), w(4, 3), t(4), v(4), p(4, 3), c(4), r(5)
integer :: i, j, k, l, total

x = [10.0, 20.0, 30.0]

! An exit of a loop inside the body.
s = 0
do concurrent (i = 1:3)
    do j = 1, 5
        if (x(i) * j > 40) exit
        s(i) = j
    end do
end do
print *, s
if (any(s /= [4, 2, 1])) error stop

! A cycle of the do concurrent.
u = 0
do concurrent (i = 1:3)
    if (x(i) > 15) cycle
    u(i) = i
end do
print *, u
if (any(u /= [1, 0, 0])) error stop

! A cycle of a do concurrent over two indices.
w = 0
do concurrent (i = 1:4, j = 1:3)
    if (mod(i + j, 2) == 0) cycle
    w(i, j) = 10 * i + j
end do
print *, w
do j = 1, 3
    do i = 1, 4
        if (mod(i + j, 2) == 0) then
            if (w(i, j) /= 0) error stop
        else
            if (w(i, j) /= 10 * i + j) error stop
        end if
    end do
end do

! A cycle and an exit of a named loop, from a loop inside it.
t = 0
do concurrent (i = 1:4)
    outer: do j = 1, 4
        do k = 1, 4
            if (k > j) cycle outer
            if (j * k > 2 * i) exit outer
            t(i) = t(i) + 1
        end do
    end do outer
end do
print *, t
if (any(t /= [2, 4, 5, 5])) error stop

! A cycle of a do concurrent nested in the offloaded one.
v = 0
p = 0
do concurrent (i = 1:4)
    v(i) = i
    do concurrent (j = 1:3)
        if (j == 2) cycle
        p(i, j) = i + j
    end do
end do
print *, v, p
if (any(v /= [1, 2, 3, 4])) error stop
if (any(p(:, 2) /= 0)) error stop
if (any(p(:, 1) /= [2, 3, 4, 5])) error stop
if (any(p(:, 3) /= [4, 5, 6, 7])) error stop

! A cycle of the do concurrent after a loop that leaves by exit.
c = -1
do concurrent (i = 1:4)
    k = 0
    do while (.true.)
        k = k + 1
        if (k >= i) exit
    end do
    if (mod(k, 2) == 0) cycle
    c(i) = k
end do
print *, c
if (any(c /= [1, -1, 3, -1])) error stop

! A cycle of a do concurrent with a reduction.
total = 0
do concurrent (i = 1:10) reduce(+:total)
    if (mod(i, 3) == 0) cycle
    total = total + i
end do
print *, total
if (total /= 37) error stop

! A cycle of the do concurrent in a SELECT CASE, and in a BLOCK after a
! loop that has a cycle of its own.
s = 0
do concurrent (i = 1:3)
    select case (i)
    case (2)
        cycle
    end select
    if (i == 3) then
        blk: block
            integer :: m
            m = 0
            do j = 1, 4
                if (j == 2) cycle
                m = m + j
            end do
            if (m > 100) cycle
            s(i) = m
        end block blk
    end if
    s(i) = s(i) + 1
end do
print *, s
if (any(s /= [1, 0, 9])) error stop

! An exit and a cycle of a named loop from a long inner loop, which must
! not be spread over the SIMD lanes of a Metal thread.
do j = 1, m
    xm(j) = real(j)
end do
xm(5) = -1.0
y = 0
do concurrent (i = 1:4)
    outer2: do j = 1, 3
        do l = 1, m
            if (xm(l) < 0) exit outer2
            y(i, l) = xm(l) * i
        end do
    end do outer2
end do
print *, count(y /= 0)
if (count(y /= 0) /= 16) error stop
if (abs(y(3, 4) - 12.0) > 1e-6) error stop
y = 0
do concurrent (i = 1:4)
    outer3: do j = 1, 3
        do l = 1, m
            if (xm(l) < 0) cycle outer3
            y(i, l) = y(i, l) + xm(l) * i
        end do
    end do outer3
end do
print *, count(y /= 0)
if (count(y /= 0) /= 16) error stop
if (abs(y(3, 4) - 36.0) > 1e-6) error stop

! An exit and a cycle in a routine the body calls.
r = 0
do concurrent (i = 1:5)
    r(i) = odd_sum(i)
end do
print *, r
if (any(r /= [1, 1, 4, 4, 9])) error stop
end program
