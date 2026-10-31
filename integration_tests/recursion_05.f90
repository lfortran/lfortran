module recursion_05_mod
implicit none

abstract interface
    subroutine callback()
    end subroutine callback
end interface

procedure(callback), pointer :: saved_proc => null()
integer, target :: targets(3) = [10, 20, 30]
integer :: ncalls = 0
integer :: pointer_seen(3) = 0
integer :: npointer_seen = 0

contains

subroutine call_saved_proc()
    call saved_proc()
end subroutine call_saved_proc

! An internal procedure called through a procedure pointer after a direct call
subroutine pointer_call()
    integer :: k
    integer :: arr(3)
    k = 5
    arr = [1, 2, 3]
    call inner()
    saved_proc => inner
    call call_saved_proc()
    if (ncalls /= 2) error stop
contains
    subroutine inner()
        if (k /= 5) error stop
        if (any(arr /= [1, 2, 3])) error stop
        ncalls = ncalls + 1
    end subroutine inner
end subroutine pointer_call

! An internal procedure passes a variable of its host to a new instance of the host
recursive subroutine pass_alloc_down(x, depth, nseen)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, intent(inout) :: nseen
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (.not. allocated(x)) error stop
        if (x(1) /= depth + 5) error stop
        nseen = nseen + 1
        if (depth > 1) then
            allocate(y(1))
            y = depth + 4
            call pass_alloc_down(y, depth - 1, nseen)
        end if
    end subroutine inner
end subroutine pass_alloc_down

recursive subroutine pass_alloc(x, depth)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (.not. allocated(x)) error stop
        x = x + 1
        if (depth > 1) then
            allocate(y(2))
            y = 10*depth
            call pass_alloc(y, depth - 1)
            if (.not. allocated(y)) error stop
            if (any(y /= 10*depth + 1)) error stop
            deallocate(y)
        end if
    end subroutine inner
end subroutine pass_alloc

recursive subroutine pass_alloc_in(x, depth, total)
    integer, allocatable, intent(in) :: x(:)
    integer, intent(in) :: depth
    integer, intent(inout) :: total
    integer, allocatable :: y(:)
    call inner()
    call inner()
    if (.not. allocated(x)) error stop
contains
    subroutine inner()
        if (.not. allocated(x)) error stop
        if (depth > 1) then
            allocate(y(depth))
            y = depth
            call pass_alloc_in(y, depth - 1, total)
            if (any(y /= depth)) error stop
            deallocate(y)
        end if
        total = total + x(1)
    end subroutine inner
end subroutine pass_alloc_in

recursive subroutine pass_scalar(n, depth)
    integer, intent(inout) :: n
    integer, intent(in) :: depth
    integer :: k
    call inner()
contains
    subroutine inner()
        n = n + 1
        if (depth > 1) then
            k = 10
            call pass_scalar(k, depth - 1)
            if (k /= 11) error stop
        end if
    end subroutine inner
end subroutine pass_scalar

recursive integer function pass_to_function(n) result(r)
    integer, intent(in) :: n
    integer :: k
    k = n
    r = inner()
contains
    integer function inner()
        inner = 0
        if (k > 1) inner = pass_to_function(k - 1) + k
        if (k /= n) error stop
    end function inner
end function pass_to_function

recursive subroutine sibling_args(n, total)
    integer, intent(in) :: n
    integer, intent(inout) :: total
    integer :: k
    k = 10*n
    call g()
    call h(n, k)
    if (k /= 10*n + 1) error stop
contains
    subroutine g()
        integer :: n0, k0
        n0 = n
        k0 = k
        if (n > 1) call sibling_args(n - 1, total)
        if (n /= n0) error stop
        if (k /= k0) error stop
        total = total + n
    end subroutine g
    subroutine h(a, b)
        integer, intent(in) :: a
        integer, intent(inout) :: b
        integer :: a0
        a0 = a
        if (n > 1) call sibling_args(n - 1, total)
        if (a /= a0) error stop
        if (b /= 10*a0) error stop
        b = b + 1
    end subroutine h
end subroutine sibling_args

! Allocatable scalars, deferred-length and assumed-length characters
recursive subroutine alloc_scalars(depth, s)
    integer, intent(in) :: depth
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: d
    integer, allocatable :: k
    allocate(k)
    k = depth
    d = repeat('x', depth)
    call inner()
    if (k /= 7*depth) error stop
    if (len(d) /= depth + 1) error stop
contains
    subroutine inner()
        if (depth > 1) call alloc_scalars(depth - 1, s // 'y')
        if (k /= depth) error stop
        if (len(d) /= depth) error stop
        if (d /= repeat('x', depth)) error stop
        if (len(s) /= 4 - depth) error stop
        k = 7*depth
        d = repeat('z', depth + 1)
    end subroutine inner
end subroutine alloc_scalars

! An allocatable array of assumed-length strings
recursive subroutine alloc_strings(depth, n)
    integer, intent(in) :: depth, n
    character(len=n), allocatable :: w(:)
    character(len=*), parameter :: abc = 'abcdef'
    allocate(w(depth))
    w = abc(1:n)
    call inner()
    if (size(w) /= depth + 1) error stop
contains
    subroutine inner()
        if (depth > 1) call alloc_strings(depth - 1, n + 1)
        if (len(w) /= n) error stop
        if (size(w) /= depth) error stop
        if (w(1) /= abc(1:n)) error stop
        deallocate(w)
        allocate(w(depth + 1))
        w = 'z'
    end subroutine inner
end subroutine alloc_strings

recursive subroutine pointers(depth)
    integer, intent(in) :: depth
    integer, pointer :: p
    integer, pointer :: pa(:)
    p => targets(depth)
    pa => targets(1:depth)
    call inner()
    if (.not. associated(p, targets(1))) error stop
contains
    subroutine inner()
        if (depth > 1) call pointers(depth - 1)
        if (p /= 10*depth) error stop
        if (size(pa) /= depth) error stop
        npointer_seen = npointer_seen + 1
        pointer_seen(npointer_seen) = p
        p => targets(1)
    end subroutine inner
end subroutine pointers

! The new instance of the host returns early
recursive subroutine early_return(depth, nseen)
    integer, intent(in) :: depth
    integer, intent(inout) :: nseen
    integer :: mine
    mine = depth
    if (depth == 1) then
        if (get() == 1) return
    end if
    call inner()
contains
    integer function get()
        get = mine
    end function get
    subroutine inner()
        call early_return(depth - 1, nseen)
        ! Only the instance of depth 2 calls inner
        if (mine /= 2 .or. depth /= 2) error stop
        nseen = nseen + 1
    end subroutine inner
end subroutine early_return

end module recursion_05_mod

program recursion_05
use recursion_05_mod
implicit none
integer, allocatable :: a(:)
integer :: n, total
call pointer_call()
allocate(a(1))
a = 7
n = 0
call pass_alloc_down(a, 2, n)
if (n /= 2) error stop
deallocate(a)
allocate(a(2))
a = 0
call pass_alloc(a, 3)
if (.not. allocated(a)) error stop
if (any(a /= 1)) error stop
a = [1, 2]
total = 0
call pass_alloc_in(a, 3, total)
if (total /= 30) error stop
if (any(a /= [1, 2])) error stop
n = 3
call pass_scalar(n, 2)
if (n /= 4) error stop
if (pass_to_function(4) /= 9) error stop
total = 0
call sibling_args(3, total)
if (total /= 11) error stop
call alloc_scalars(3, 'a')
call alloc_strings(3, 2)
call pointers(3)
if (any(pointer_seen /= [10, 20, 30])) error stop
n = 0
call early_return(2, n)
if (n /= 1) error stop
print *, "ok"
end program recursion_05
