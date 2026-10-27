module recursion_04_mod
implicit none

type :: pair_t
    integer :: a = 0
    integer :: b = 0
end type pair_t

integer :: seen(3) = 0
integer :: nseen = 0
integer :: seen_indirect(3) = 0
integer :: nseen_indirect = 0
integer :: seen_assumed(3) = 0
integer :: nseen_assumed = 0
integer :: seen_class(3) = 0
integer :: nseen_class = 0

contains

recursive subroutine outer(depth)
    integer, intent(in) :: depth
    integer :: mine
    mine = depth
    call inner()
    if (mine /= 10*depth) error stop
contains
    subroutine inner()
        if (depth > 1) call outer(depth - 1)
        if (mine /= depth) error stop
        nseen = nseen + 1
        seen(nseen) = depth
        mine = 10*depth
    end subroutine inner
end subroutine outer

recursive subroutine arrays(depth)
    integer, intent(in) :: depth
    integer :: fixed(2)
    integer, allocatable :: alloc(:)
    character(len=4) :: str
    type(pair_t) :: p
    fixed = [depth, -depth]
    allocate(alloc(depth))
    alloc = depth
    write(str, '(a,i3)') 'd', depth
    p%a = depth
    p%b = 2*depth
    call check()
    if (any(fixed /= [depth + 1, -depth])) error stop
    if (size(alloc) /= depth + 1) error stop
    if (any(alloc /= 100*depth)) error stop
    if (str /= 'done') error stop
    if (p%a /= depth .or. p%b /= 3*depth) error stop
contains
    subroutine check()
        character(len=4) :: expected
        if (depth > 1) call arrays(depth - 1)
        write(expected, '(a,i3)') 'd', depth
        if (any(fixed /= [depth, -depth])) error stop
        if (size(alloc) /= depth) error stop
        if (any(alloc /= depth)) error stop
        if (str /= expected) error stop
        if (p%a /= depth .or. p%b /= 2*depth) error stop
        fixed(1) = depth + 1
        deallocate(alloc)
        allocate(alloc(depth + 1))
        alloc = 100*depth
        str = 'done'
        p%b = 3*depth
    end subroutine check
end subroutine arrays

recursive integer function sum_down(n) result(r)
    integer, intent(in) :: n
    integer :: here
    here = n
    r = get()
contains
    integer function get()
        get = 0
        if (here > 1) get = sum_down(here - 1)
        get = get + here
    end function get
end function sum_down

subroutine call_back(f)
    interface
        subroutine f()
        end subroutine f
    end interface
    call f()
end subroutine call_back

recursive subroutine indirect(depth)
    integer, intent(in) :: depth
    integer :: mine
    mine = depth
    call call_back(inner)
contains
    subroutine inner()
        if (depth > 1) call indirect(depth - 1)
        if (mine /= depth) error stop
        nseen_indirect = nseen_indirect + 1
        seen_indirect(nseen_indirect) = depth
    end subroutine inner
end subroutine indirect

recursive subroutine assumed_size(depth, a)
    integer, intent(in) :: depth
    integer, intent(in) :: a(*)
    integer :: b(3)
    b = depth
    call inner()
contains
    subroutine inner()
        if (depth > 1) call assumed_size(depth - 1, b)
        if (a(1) /= depth + 1) error stop
        if (any(b /= depth)) error stop
        nseen_assumed = nseen_assumed + 1
        seen_assumed(nseen_assumed) = a(1)
    end subroutine inner
end subroutine assumed_size

recursive subroutine class_host(depth, x)
    integer, intent(in) :: depth
    class(*), intent(inout) :: x(:)
    integer :: y(2)
    y = depth
    call inner()
contains
    subroutine inner()
        if (depth > 1) call class_host(depth - 1, y)
        select type (x)
        type is (integer)
            nseen_class = nseen_class + 1
            seen_class(nseen_class) = x(1)
            x = 2*x
        end select
    end subroutine inner
end subroutine class_host

end module recursion_04_mod

program recursion_04
use recursion_04_mod
implicit none
call outer(3)
if (nseen /= 3) error stop
if (any(seen /= [1, 2, 3])) error stop
call arrays(3)
if (sum_down(4) /= 10) error stop
call indirect(3)
if (nseen_indirect /= 3) error stop
if (any(seen_indirect /= [1, 2, 3])) error stop
call assumed_size(3, [4])
if (nseen_assumed /= 3) error stop
if (any(seen_assumed /= [2, 3, 4])) error stop
block
    integer :: c(2)
    c = [10, 20]
    call class_host(3, c)
    if (nseen_class /= 3) error stop
    if (any(seen_class /= [2, 3, 10])) error stop
    if (any(c /= [20, 40])) error stop
end block
print *, "ok"
end program recursion_04
