! A scalar a `do concurrent` of one iteration assigns keeps that value
! after the loop (Fortran 2018, 11.1.7.5), and a later statement may read
! it without naming it, or past a later assignment to it: through an
! ASSOCIATE name or an EQUIVALENCE, in a BLOCK's declaration, in a
! DO WHILE condition, or after a CYCLE or an EXIT. The host has to read
! it back even though a later statement assigns it first.
program gpu_liveout_17
    implicit none
    real(8) :: t, u, e1, e2
    integer :: i, k, it, m, n2
    equivalence (e1, e2)
    m = 1

    ! An ASSOCIATE name of it.
    t = 0
    associate (a => t)
        do concurrent (i = 1:m)
            t = 50
        end do
        u = a
    end associate
    t = 0
    if (u /= 50) error stop 1

    ! An EQUIVALENCE partner of it.
    e2 = 0
    do concurrent (i = 1:m)
        e2 = 61
    end do
    u = e1
    e2 = 0
    if (u /= 61) error stop 3

    ! The declaration of a later BLOCK.
    n2 = 0
    do concurrent (i = 1:m)
        n2 = 4*i
    end do
    block
        real(8) :: w(n2)
        w = 1
        u = sum(w)
    end block
    n2 = 0
    if (u /= 4) error stop 4

    ! The condition of a later DO WHILE.
    t = 0
    do concurrent (i = 1:m)
        t = 3
    end do
    k = 0
    do while (t > 2 .and. k < 5)
        k = k + 1
        t = 0
    end do
    if (k /= 1) error stop 5

    ! A CYCLE skips the assignment, and the next trip reads it.
    t = 0
    u = 0
    do it = 1, 2
        if (it == 2) u = t
        do concurrent (i = 1:m)
            t = 30
        end do
        if (it == 1) cycle
        t = 0
    end do
    if (u /= 30) error stop 6

    ! An EXIT out of two loops skips the assignment.
    t = 0
    outer: do it = 1, 2
        do k = 1, 2
            do concurrent (i = 1:m)
                t = 40
            end do
            if (k == 1) exit outer
        end do
        t = 0
    end do outer
    u = t
    t = 0
    if (u /= 40) error stop 7
    print *, u, n2, k
end program
