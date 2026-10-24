! Variables that one iteration of a `do concurrent` defines in an inner
! loop, where the kernel puts a thread's copy back right after each
! definition rather than once after the statement holding the inner loop:
! in an inner loop left by EXIT, in a statement with a CYCLE of the `do
! concurrent`, after which nothing more of the iteration runs, and in one
! that can STOP. Elements of an array component an inner loop defines
! stay in place.
module gpu_liveout_08_m
    implicit none
    integer, parameter :: n = 100
    type :: g_t
        real, allocatable :: w(:)
        integer :: c(3 * n)
    end type
end module

program gpu_liveout_08
    use gpu_liveout_08_m
    implicit none
    integer :: e, m, s, i, j
    integer :: a(n)
    type(g_t) :: g
    allocate(g%w(n))
    g%w = 1
    e = 0
    m = 0
    s = 0
    do concurrent (i = 1:n)
        if (i == 3) then
            do j = 1, 10
                e = e + j
                if (e > 20) exit
            end do
        end if
        a(i) = i
        do j = 1, 3
            g%c(3 * (i - 1) + j) = 10 * i + j
        end do
        if (i == 5) then
            do j = 1, 3
                m = 50 + j
            end do
            cycle
        end if
        a(i) = a(i) + 1
        if (i == 9) then
            do j = 1, 2
                s = 90 + j
                if (s < 0) error stop 9
            end do
        end if
    end do
    print *, e, m, s, a(5), a(6), g%c(13), g%c(18)
    if (e /= 21) error stop 1
    if (m /= 53) error stop 2
    if (s /= 92) error stop 3
    if (a(5) /= 5 .or. a(6) /= 7) error stop 4
    if (g%c(13) /= 51 .or. g%c(18) /= 63) error stop 5
end program
