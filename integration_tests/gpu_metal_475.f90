! Which arrays of unspecified locality a do concurrent loop on the GPU gives
! each iteration its own copy of (issue #15307, see gpu_metal_474). An array
! a statement after the loop may read -- however a jump gets there -- or that
! an iteration reads before defining it all, keeps the one shared value the
! source gives it, as does one too large for what a thread's stack has left
! and one of a size only known at run time.
program gpu_metal_475
implicit none
integer, parameter :: n = 10000, big = 16384, near = 8192, mid = 5000
real :: y(n), a(4), b(4), c(4), d(4), e(4), f(4), g(4), h(8), p(near)
real :: q(big), v(4), w(8), o(4), r(4), s(4), u(4), x(mid), z(n)
integer :: i, j, k, it
logical :: stop_now

stop_now = .true.

! An exit after the loop skips the definition that follows it.
a = -1
do i = 1, 2
    do concurrent (j = 1:1)
        a = 10. * j
        y(j) = sum(a)
    end do
    if (stop_now) exit
    do k = 1, 4
        a(k) = 0
    end do
end do
if (any(a /= 10)) error stop 1

! So does an exit of an outer loop, from a loop of its own.
b = -1
outer: do i = 1, 2
    do concurrent (j = 1:1)
        b = 20. * j
        y(j) = sum(b)
    end do
    do k = 1, 3
        if (stop_now) exit outer
    end do
    do k = 1, 4
        b(k) = 0
    end do
end do outer
if (any(b /= 20)) error stop 2

! A cycle skips it on the last iteration.
c = -1
do i = 1, 2
    do concurrent (j = 1:1)
        c = 30. * i
        y(j) = sum(c)
    end do
    if (i == 2) cycle
    do k = 1, 4
        c(k) = 0
    end do
end do
if (any(c /= 60)) error stop 3

! An exit of a BLOCK construct skips it too.
d = -1
blk: block
    do concurrent (j = 1:1)
        d = 40. * j
        y(j) = sum(d)
    end do
    if (stop_now) exit blk
    do k = 1, 4
        d(k) = 0
    end do
end block blk
if (any(d /= 40)) error stop 4

! The condition of a do while reads the array before the body runs again.
e = 1
it = 0
do while (e(1) > 0)
    it = it + 1
    if (it > 10) error stop 5
    do concurrent (j = 1:1)
        e = 3 - it
        y(j) = sum(e)
    end do
end do
if (it /= 3) error stop 6

! An iteration that defines one element and reads another: the other keeps
! the value from before the loop.
f = 7
do concurrent (j = 1:n)
    if (j == 3) f(1) = 5
    y(j) = f(2) + j
end do
do j = 1, n
    if (y(j) /= 7 + j) error stop 7
end do

! The one iteration that defines the array reads it first.
g = 8
do concurrent (j = 1:n)
    if (j == 5) then
        y(j) = g(1)
        g = 0
    else
        y(j) = j
    end if
end do
if (y(5) /= 8 .or. y(6) /= 6) error stop 8

! An array that only some iterations define, each before using it, is a
! temporary of each of them.
do concurrent (j = 1:n)
    if (mod(j, 2) == 0) then
        h = j
        do k = 1, 8
            h(k) = h(k) + k
        end do
        y(j) = sum(h)
    else
        y(j) = -1
    end if
end do
do j = 1, n
    if (mod(j, 2) == 0) then
        if (y(j) /= 8 * j + 36) error stop 9
    else
        if (y(j) /= -1) error stop 10
    end if
end do

! A temporary of 32 KiB fits a thread's stack.
do concurrent (j = 1:256)
    do k = 1, near
        p(k) = j + k
    end do
    y(j) = p(1) + p(near)
end do
do j = 1, 256
    if (y(j) /= 2 * j + 1 + near) error stop 11
end do

! An array of 64 KiB does not on Metal: it stays one array, which is right
! here, as one iteration defines it.
do concurrent (j = 1:1000)
    if (j == 7) then
        do k = 1, big
            q(k) = k + j
        end do
        y(j) = q(big)
    else
        y(j) = 0
    end if
end do
if (y(7) /= big + 7 .or. y(1) /= 0) error stop 12

! A contained function the body calls does not reach the temporary.
do concurrent (j = 1:n)
    w = twice(j)
    y(j) = sum(w)
end do
do j = 1, n
    if (y(j) /= 16 * j) error stop 13
end do

! A contained subroutine that names the array reads it after the loop.
v = -1
do concurrent (j = 1:1)
    v = 50. * j
    y(j) = sum(v)
end do
call check_v()

! The temporary is a local of a BLOCK the body becomes; an ASSOCIATE or a
! BLOCK of the body goes into it too.
do concurrent (j = 1:n)
    o = j
    y(j) = sum(o)
    associate (al => y(j))
        al = al + 1
    end associate
end do
do j = 1, n
    if (y(j) /= 4 * j + 1) error stop 15
end do

do concurrent (j = 1:n)
    r = j
    associate (al => y(j))
        al = sum(r)
    end associate
end do
do j = 1, n
    if (y(j) /= 4 * j) error stop 16
end do

do concurrent (j = 1:n)
    block
        s = j
        y(j) = sum(s)
    end block
end do
do j = 1, n
    if (y(j) /= 4 * j) error stop 17
end do

do concurrent (j = 1:n)
    u = j
    block
        associate (al => y(j))
            al = sum(u)
        end associate
    end block
end do
do j = 1, n
    if (y(j) /= 4 * j) error stop 18
end do

! A temporary of 20,000 bytes does not fit next to a BLOCK local of 40,000
! on a Metal stack of 64 KiB: it stays one array, which one iteration defines.
do concurrent (j = 1:1000)
    if (j == 7) then
        do k = 1, mid
            x(k) = k + j
        end do
        y(j) = x(mid)
    else
        y(j) = 0
    end if
    block
        real :: wl(10000)
        wl = j
        z(j) = wl(10000)
    end block
end do
if (y(7) /= mid + 7 .or. y(1) /= 0 .or. z(5) /= 5) error stop 19

! Next to one of 24,000 it does, so every iteration defines its own.
do concurrent (j = 1:256)
    do k = 1, mid
        x(k) = k + j
    end do
    block
        real :: wl(6000)
        wl = j
        y(j) = x(mid) + wl(6000)
    end block
end do
do j = 1, 256
    if (y(j) /= mid + 2 * j) error stop 20
end do

! A temporary of a size only known at run time stays one array: its own
! copy for every iteration would take 400 kB each.
call run_time_size(100000, 200000)
print *, "ok"

contains

    pure real function twice(j)
        integer, intent(in) :: j
        twice = 2 * j
    end function

    subroutine check_v()
        if (any(v /= 50)) error stop 14
    end subroutine

    subroutine run_time_size(m, nr)
        integer, intent(in) :: m, nr
        real :: t(m), yr(nr)
        integer :: jr, kr
        do concurrent (jr = 1:nr)
            if (jr == 7) then
                do kr = 1, m
                    t(kr) = kr + jr
                end do
                yr(jr) = t(m)
            else
                yr(jr) = 0
            end if
        end do
        if (yr(7) /= m + 7 .or. yr(1) /= 0) error stop 21
    end subroutine
end program
