program doloop_shared_label_goto
! A branch to the shared terminal statement of nested DO loops from outside
! the innermost loop. Compilers accept it as a legacy extension at most, so
! this is only printed, not run: the label must still be printed, once.
implicit none
integer :: i, j, n
n = 0
do 10 i = 1, 3
    if (i == 2) go to 10
    do 10 j = 1, 2
        n = n + 1
10 continue
print *, n
end program
