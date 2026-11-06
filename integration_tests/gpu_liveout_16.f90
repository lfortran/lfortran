! A scalar of unspecified locality that every iteration of a `do concurrent`
! assigns before referencing it is undefined after the loop (Fortran 2018,
! 11.1.7.5) when more than one iteration ran. When the statements after the
! loop that name it all assign it first (here the next loop), nothing reads
! the value it is left with, so the host does not need it back. When one of
! them may read it, the value is kept: a loop of one iteration defines it.
program gpu_liveout_16
    implicit none
    integer, parameter :: nr = 8, np = 16
    real :: x(nr,np), s0(nr), s1(nr), t, u
    integer :: it, i, k, m
    x = 1
    do it = 1, 3
        do concurrent (k = 1:np, i = 1:nr)
            x(i,k) = x(i,k) + i
        end do
        do concurrent (i = 1:nr)
            t = 0
            do k = 1, np
                t = t + x(i,k)
            end do
            s0(i) = t
        end do
        do concurrent (i = 1:nr)
            t = 0
            do k = 1, np
                t = t + 2*x(i,k)
            end do
            s1(i) = t
        end do
    end do
    do i = 1, nr
        if (abs(s0(i) - np*(1 + 3*i)) > 1e-6) error stop 1
        if (abs(s1(i) - 2*np*(1 + 3*i)) > 1e-6) error stop 2
    end do

    ! One iteration: the value of `u` it defines is read after the loop.
    m = nr - 7
    do concurrent (i = 1:m)
        u = 0
        do k = 1, np
            u = u + x(i,k)
        end do
        s0(i) = u
    end do
    if (abs(u - np*4) > 1e-6) error stop 3

    ! Read in one branch of a later IF, assigned first in the other.
    do concurrent (i = 1:m)
        t = 5*i
    end do
    if (m > 1) then
        t = 0
        u = 1
    else
        u = t
    end if
    if (u /= 5) error stop 4

    ! An EXIT skips the assignment that would replace the value.
    do it = 1, 3
        do concurrent (i = 1:m)
            t = 7 + it
        end do
        if (it == 2) exit
        t = 0
    end do
    if (t /= 9) error stop 5

    ! A later loop reads the value before anything assigns it.
    do concurrent (i = 1:m)
        t = 11
    end do
    do concurrent (i = 1:nr)
        s1(i) = t
    end do
    t = 0
    if (s1(nr) /= 11) error stop 6

    ! The next trip of a loop around it reads the value first.
    u = 0
    t = 0
    do it = 1, 3
        u = u + t
        do concurrent (i = 1:m)
            t = it
        end do
    end do
    if (u /= 3) error stop 7
    print *, s0(1), s1(1), u
end program
