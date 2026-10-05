program doloop_24
! A branch to the shared terminal statement of nested DO loops from outside
! the innermost loop (a legacy extension): it ends the current iteration of
! the loop it is in. The label must still be printed, once, by --show-ast-f90.
implicit none
integer :: i, j, k, n
n = 0
do 10 i = 1, 3
    if (i == 2) go to 10
    do 10 j = 1, 2
        n = n + 1
10 continue
print *, n
if (n /= 4) error stop
! From the middle loop of three
n = 0
do 20 i = 1, 2
    do 20 j = 1, 3
        if (j == 2) go to 20
        do 20 k = 1, 2
            n = n + 1
20 continue
print *, n
if (n /= 8) error stop
! From the outermost loop of three
n = 0
do 30 i = 1, 3
    if (i == 2) go to 30
    do 30 j = 1, 2
        do 30 k = 1, 2
            n = n + 1
30 continue
print *, n
if (n /= 8) error stop
end program
