program doloop_shared_term_branch_01
! The terminal statement shared by nested DO loops can only be branched to
! from the innermost of them (F2008 8.1.6.4): a branch from an outer loop
! is accepted with a warning.
implicit none
integer :: i, j, k, n, u
n = 0
u = 10

! from the outer loop: warning
do 10 i = 1, 3
    if (i == 2) go to 10
    do 10 j = 1, 2
        n = n + 1
10 continue

! from the middle loop of a three-deep nest: warning
do 20 i = 1, 2
    do 20 j = 1, 2
        if (j == 2) go to 20
        do 20 k = 1, 2
            n = n + 1
20 continue

! from the innermost loop: no warning
do 30 i = 1, 2
    do 30 j = 1, 2
        do 30 k = 1, 2
            if (k == 2) go to 30
            n = n + 1
30 continue

! the terminal statement is an action statement: warning
do 40 i = 1, 2
    if (i == 2) go to 40
    do 40 j = 1, 2
40 n = n + 1

! each loop has its own terminal statement: no warning
do 60 i = 1, 2
    do 50 j = 1, 2
        if (j == 2) go to 50
        n = n + 1
50  continue
    if (i == 2) go to 60
    n = n + 1
60 continue

! computed GO TO and arithmetic IF: warnings
do 70 i = 1, 2
    go to (70, 71), i
71  if (i - 2) 70, 72, 70
72  do 70 j = 1, 2
        n = n + 1
70 continue

! ERR= and END= specifiers: one warning per statement
do 80 i = 1, 2
    open(u, file="doloop_shared_term_branch_01.txt", err=80)
    read(u, *, end=80, err=80) k
    do 80 j = 1, 2
        n = n + 1
80 continue

! alternate returns: one warning per statement
do 90 i = 1, 2
    call alt(i, *90, *90)
    do 90 j = 1, 2
        n = n + 1
90 continue

print *, n

contains

subroutine alt(i, *, *)
    integer, intent(in) :: i
    return i
end subroutine

end program
