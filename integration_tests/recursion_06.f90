program recursion_06
use recursion_06_mod
implicit none
integer, allocatable :: a(:)
integer :: k
type(solver) :: s

allocate(a(3))
a = 7
call elem_arg(a, a(1), 3)
if (any(a /= [7, 8, 7])) error stop

deallocate(a)
allocate(a(2))
a = [1, 2]
call sibling_arg(a, sum(a), 3)
if (any(a /= [2, 2])) error stop

a = [1, 2]
if (sum_host(a, 3) /= 3) error stop
if (any(a /= [1, 2])) error stop

k = 5
if (count_up(k, 3) /= 6) error stop
if (k /= 6) error stop

s%id = 4
a = [7, 7]
call s%solve(a, 3)
if (any(a /= 8)) error stop

call pointer_after_recursion(3)
if (any(seen /= 2)) error stop
print *, "ok"
end program recursion_06
