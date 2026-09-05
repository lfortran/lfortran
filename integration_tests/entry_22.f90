! Test that a statement label inside the part of a subprogram that is shared
! between the main entry point and an ENTRY does not confuse code generation.
! The body after the ENTRY is emitted once per entry point, so the label is
! defined more than once.
program entry_22
    implicit none
    integer :: r, m(2,2)

    call sub(3, r)
    if (r /= 12) error stop
    call ent(4, r)
    if (r /= 20) error stop

    m = reshape([1, 2, 3, 4], [2, 2])
    call sn510(2, m, r)
    if (r /= 10) error stop
    call en856(2, m, r)
    if (r /= 10) error stop

    print *, "OK"
end program

subroutine sub(n, r)
    implicit none
    integer :: n, r, i
    entry ent(n, r)
    r = 0
    do 10 i = 1, n
        r = r + i
10  continue
    if (r > 0) go to 20
    r = -1
20  r = r * 2
end subroutine

! A labeled DO loop whose terminating statement is not CONTINUE, nested in an
! unlabeled DO loop, reachable from two entry points.
subroutine sn510(n, a, r)
    implicit none
    integer :: n, r, a(2,2), i, j
    entry en856(n, a, r)
    r = 0
    do i = 1, n
    do 70020 j = 1, n
70020 r = r + a(i,j)
    end do
end subroutine
