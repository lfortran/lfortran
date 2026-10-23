! A scalar the host reads after a `do concurrent` that one iteration
! defines by passing it to a procedure that defines it keeps that value
! (Fortran 2018, 11.1.7.5), and one every iteration defines that way and
! then reads is a temporary of the iteration.
module gpu_liveout_03_m
    implicit none
contains
    pure subroutine incr(k)
        integer, intent(inout) :: k
        k = k + 1
    end subroutine

    pure subroutine set(k, v)
        integer, intent(out) :: k
        integer, intent(in) :: v
        k = v
    end subroutine
end module

program gpu_liveout_03
    use gpu_liveout_03_m
    implicit none
    integer, parameter :: n = 100000
    integer :: k, t, i
    integer, allocatable :: a(:)
    allocate(a(n))
    k = 0
    do concurrent (i = 1:n)
        a(i) = i
        if (i == 7) then
            k = 10
            call incr(k)
        end if
    end do
    print *, k
    if (k /= 11) error stop 1
    t = 0
    do concurrent (i = 1:n)
        call set(t, 2 * i)
        a(i) = t - i
    end do
    do i = 1, n
        if (a(i) /= i) error stop 2
    end do
    t = 3
    print *, t, a(n)
end program
