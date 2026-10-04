program doloop_24
! A branch to the shared terminal statement of nested DO loops from outside
! the innermost loop (a legacy extension): it ends the current iteration of
! the outer loop. The label must still be printed, once, by --show-ast-f90.
implicit none
integer :: i, j, n
n = 0
do 10 i = 1, 3
    if (i == 2) go to 10
    do 10 j = 1, 2
        n = n + 1
10 continue
print *, n
if (n /= 4) error stop
end program
