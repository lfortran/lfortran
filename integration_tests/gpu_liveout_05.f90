! A module variable that every iteration of a `do concurrent` accumulates
! into in an inner loop is a temporary of the iteration (Fortran 2018,
! 11.1.7.5): each iteration reads its own value. The kernel puts the
! value back where it is shared once, after the inner loop, not on every
! trip of it.
module gpu_liveout_05_m
    implicit none
    real :: tmp
contains
    subroutine s(n, a)
        integer, intent(in) :: n
        real, intent(inout) :: a(n)
        integer :: i, j
        do concurrent (i = 1:n)
            tmp = 0
            do j = 1, 200
                tmp = tmp + a(i) * j
            end do
            a(i) = tmp
        end do
    end subroutine
end module

program gpu_liveout_05
    use gpu_liveout_05_m
    implicit none
    integer, parameter :: n = 100000
    real, allocatable :: a(:)
    integer :: i
    allocate(a(n))
    do i = 1, n
        a(i) = mod(i, 7)
    end do
    call s(n, a)
    do i = 1, n
        if (a(i) /= 20100 * mod(i, 7)) error stop 1
    end do
    print *, a(1), a(n)
end program
