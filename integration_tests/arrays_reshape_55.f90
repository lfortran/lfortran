module arrays_reshape_55_m
implicit none
contains
    subroutine fill(w)
        integer, intent(inout) :: w(0:)
        integer :: k
        w = reshape([(k, k = 1, 3)], [3])
        if (lbound(w, 1) /= 0) error stop 3
        if (any(w /= [1, 2, 3])) error stop 4
    end subroutine
end module

program arrays_reshape_55
use arrays_reshape_55_m, only: fill
implicit none
integer, allocatable :: m(:, :)
integer :: a(6), k

allocate(m(0:1, -1:1))
m = reshape([(k, k = 1, 6)], [2, 3])
if (any(lbound(m) /= [0, -1])) error stop 1
if (any(m(:, 1) /= [5, 6])) error stop 2

a = 0
call fill(a(1:6:2))
if (any(a /= [1, 0, 2, 0, 3, 0])) error stop 5
print *, "ok"
end program
