! A scalar and a derived-type local, not read after a `do concurrent`,
! that the loop passes to an `intent(out)` dummy argument in a call that
! does not run in any iteration, and reads: they keep the values they had
! before the loop.
module gpu_liveout_13_m
    implicit none
    type :: w_t
        real :: a, c
    end type
contains
    pure subroutine init(t, v)
        real, intent(out) :: t
        real, intent(in) :: v
        t = v * 2
    end subroutine

    pure subroutine init_w(w, v)
        type(w_t), intent(out) :: w
        real, intent(in) :: v
        w%a = v
        w%c = v
    end subroutine

    subroutine run(x, y)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        type(w_t) :: w
        integer :: i
        w%a = 1
        w%c = 2
        do concurrent (i = 1:size(x))
            if (x(i) > 1000) call init_w(w, x(i))
            y(i) = w%a + w%c
        end do
    end subroutine
end module

program gpu_liveout_13
    use gpu_liveout_13_m
    implicit none
    integer, parameter :: n = 100
    real :: x(n), y(n), t
    integer :: i
    do i = 1, n
        x(i) = i
    end do
    t = 5
    do concurrent (i = 1:n)
        if (x(i) > 1000) call init(t, x(i))
        y(i) = t + x(i)
    end do
    print *, y(7)
    if (y(7) /= 12) error stop 1
    call run(x, y)
    print *, y(7)
    if (y(7) /= 3) error stop 2
end program
