module recursion_08_mod
implicit none
contains

! The operation of `reduce` enters `host_fn` again
pure recursive integer function op_fn(a, b) result(c)
    integer, intent(in) :: a, b
    c = a + b
    if (a > 1) c = c + 0*host_fn(a - 1)
end function op_fn

pure recursive integer function host_fn(depth) result(r)
    integer, intent(in) :: depth
    integer :: mine
    mine = 10*depth
    r = inner()
contains
    pure integer function inner() result(v)
        integer :: s
        s = reduce([depth, 0], op_fn)
        if (s /= depth) error stop 1
        v = mine
    end function inner
end function host_fn

! The operation of `reduce` enters `host_sub` again
pure recursive integer function op_sub(a, b) result(c)
    integer, intent(in) :: a, b
    integer :: t(2)
    c = a + b
    if (a > 1) then
        call host_sub(a - 1, t)
        c = c + 0*t(1)
    end if
end function op_sub

pure recursive subroutine host_sub(depth, res)
    integer, intent(in) :: depth
    integer, intent(out) :: res(2)
    integer :: mine(2)
    mine = [depth, 2*depth]
    call inner(res)
contains
    pure subroutine inner(out)
        integer, intent(out) :: out(2)
        integer :: s
        s = reduce([depth, 0], op_sub)
        if (s /= depth) error stop 2
        out = mine
    end subroutine inner
end subroutine host_sub

end module recursion_08_mod

program recursion_08
use recursion_08_mod, only: host_fn, host_sub
implicit none
integer :: r, res(2)

r = host_fn(3)
print *, r
if (r /= 30) error stop 3

call host_sub(3, res)
print *, res
if (any(res /= [3, 6])) error stop 4

print *, "ok"
end program recursion_08
