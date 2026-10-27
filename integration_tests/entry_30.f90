! An ALLOCATE with SOURCE= in a procedure with ENTRY must run in place,
! after the assignment to its source, in every copy of the body.
subroutine entry_30_s(k)
    implicit none
    integer :: k, b
    integer, allocatable :: a
    b = 7
    allocate(a, source=b)
    k = a
    return
    entry entry_30_e(k)
    b = 9
    allocate(a, source=b)
    if (a /= 9) error stop 3
    k = -1
end subroutine

program entry_30
    implicit none
    integer :: k
    call entry_30_s(k)
    print '(i0)', k
    if (k /= 7) error stop 1
    call entry_30_e(k)
    print '(i0)', k
    if (k /= -1) error stop 2
end program
