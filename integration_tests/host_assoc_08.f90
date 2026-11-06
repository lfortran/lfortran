! An internal procedure and a new instance of its recursive host in the same
! statement, and host variables passed to a new instance of the host
module host_assoc_08_m
implicit none
contains

! The internal function and the host in one expression, in either order
recursive function same_stmt(n) result(r)
    integer, intent(in) :: n
    integer :: r, x
    x = 10*n
    if (n == 0) then
        r = 0
        return
    end if
    r = get_x() + same_stmt(n - 1)
    if (x /= 10*n) error stop 1
    r = same_stmt(n - 1) + get_x()
    if (x /= 10*n) error stop 2
contains
    integer function get_x()
        get_x = x
    end function get_x
end function same_stmt

! The internal function is an actual argument of the call to the host
recursive subroutine call_arg(n, acc, add)
    integer, intent(in) :: n, add
    integer, intent(inout) :: acc
    integer :: x
    x = 10*n
    acc = acc + add
    if (n == 0) return
    call call_arg(n - 1, acc, get_x())
    if (x /= 10*n) error stop 3
contains
    integer function get_x()
        get_x = x
    end function get_x
end subroutine call_arg

! In a WHERE statement and in the test of an IF construct
recursive function where_if(n) result(r)
    integer, intent(in) :: n
    integer :: r, x
    x = 10*n
    r = 0
    if (n == 0) return
    block
        integer :: arr(2)
        arr = [1, 2]
        where (arr > 0) arr = get_x() + where_if(n - 1)
        r = arr(1)
    end block
    if (x /= 10*n) error stop 4
    if (get_x() > where_if(n - 1) + 1000) then
        r = 0
    else
        r = x + where_if(n - 1)
    end if
    if (x /= 10*n) error stop 5
contains
    integer function get_x()
        get_x = x
    end function get_x
end function where_if

! A host allocatable passed down to a new instance of the host
recursive subroutine alloc_down(x, depth, nseen)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, intent(inout) :: nseen
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (.not. allocated(x)) error stop 6
        nseen = nseen + 1
        if (depth > 1) then
            allocate(y(1))
            y = 7
            call alloc_down(y, depth - 1, nseen)
            if (.not. allocated(y)) error stop 7
            if (y(1) /= 8) error stop 8
        end if
        x = x + 1
    end subroutine
end subroutine alloc_down

! A host scalar passed down to a new instance of the host
recursive subroutine scalar_down(n, depth)
    integer, intent(inout) :: n
    integer, intent(in) :: depth
    integer :: k
    call inner()
contains
    subroutine inner()
        n = n + 1
        if (depth > 1) then
            k = 10
            call scalar_down(k, depth - 1)
            if (k /= 11) error stop 9
        end if
    end subroutine
end subroutine scalar_down

! An intent(in) allocatable dummy, and a host allocatable passed down to it
recursive subroutine alloc_in(x, depth, total)
    integer, allocatable, intent(in) :: x(:)
    integer, intent(in) :: depth
    integer, intent(inout) :: total
    integer, allocatable :: y(:)
    call inner()
    call inner()
    if (.not. allocated(x)) error stop 10
contains
    subroutine inner()
        if (.not. allocated(x)) error stop 11
        if (depth > 1) then
            allocate(y(depth))
            y = depth
            call alloc_in(y, depth - 1, total)
            deallocate(y)
        end if
        total = total + size(x)
    end subroutine
end subroutine alloc_in

! The host is called with an element and the whole of a host allocatable
recursive integer function elem_target(x, depth) result(r)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, allocatable :: y(:)
    integer :: s
    r = 0
    call inner()
contains
    subroutine inner()
        r = sum(x)
        if (depth > 1) then
            allocate(y(2))
            y = [depth, depth]
            y(1) = elem_target(y, depth - 1)
            if (.not. allocated(y)) error stop 12
            if (any(y /= [2*depth, depth])) error stop 13
            s = sum(y) + elem_target(y, depth - 1)
            if (s /= 6*depth) error stop 14
        end if
    end subroutine inner
end function elem_target

recursive subroutine elem_arg(x, s, depth)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: s, depth
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (s /= x(1)) error stop 15
        if (depth > 1) then
            allocate(y(3))
            y = depth
            call elem_arg(y, y(1), depth - 1)
        end if
    end subroutine inner
end subroutine elem_arg

! A target dummy of the host
recursive subroutine target_dummy(x, depth)
    integer, target, intent(inout) :: x
    integer, intent(in) :: depth
    integer :: k
    k = 10*depth
    call inner()
contains
    subroutine inner()
        x = x + 1
        if (depth > 1) then
            call target_dummy(k, depth - 1)
            if (k /= 10*depth + 1) error stop 16
        end if
    end subroutine inner
end subroutine target_dummy

! A host allocatable forwarded to the host through a module procedure
recursive subroutine forward(z, depth, s)
    integer, allocatable, intent(inout) :: z(:)
    integer, intent(in) :: depth
    integer, intent(out) :: s
    call forward_host(z, depth, s)
end subroutine forward

recursive subroutine forward_host(x, depth, s)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, intent(out) :: s
    integer, allocatable :: y(:)
    integer :: t
    s = 0
    call inner()
contains
    subroutine inner()
        s = sum(x)
        if (depth > 1) then
            allocate(y(3))
            y = depth
            call forward(y, depth - 1, t)
            if (t /= 3*depth) error stop 17
            s = s + t
        end if
    end subroutine inner
end subroutine forward_host

end module host_assoc_08_m

program host_assoc_08
use host_assoc_08_m
implicit none
integer :: acc, n, total, s
integer, allocatable :: a(:)
if (same_stmt(3) /= 60) error stop 20
acc = 0
call call_arg(3, acc, 0)
if (acc /= 60) error stop 21
if (where_if(3) /= 60) error stop 22
allocate(a(1))
a = 3
n = 0
call alloc_down(a, 2, n)
if (n /= 2 .or. a(1) /= 4) error stop 23
n = 3
call scalar_down(n, 2)
if (n /= 4) error stop 24
deallocate(a)
allocate(a(3))
a = [1, 2, 3]
total = 0
call alloc_in(a, 3, total)
if (total /= 34) error stop 25
if (any(a /= [1, 2, 3])) error stop 26
deallocate(a)
allocate(a(1))
a = 7
if (elem_target(a, 2) /= 7) error stop 27
deallocate(a)
allocate(a(2))
a = 7
call elem_arg(a, a(1), 2)
n = 5
call target_dummy(n, 3)
if (n /= 6) error stop 28
a = [1, 2]
call forward_host(a, 2, s)
if (s /= 9) error stop 29
print *, "ok"
end program host_assoc_08
