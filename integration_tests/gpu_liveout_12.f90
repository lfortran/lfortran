! A derived-type variable and a scalar passed to `intent(inout)` dummy
! arguments of procedures called in a `do concurrent`, which only read
! them: the loop may define them but does not, so they keep the values
! they had before it, although they are not read after it.
module gpu_liveout_12_m
    implicit none
    type :: par_t
        real :: c
        integer :: ncalls
    end type
contains
    pure subroutine step(p, x, r)
        type(par_t), intent(inout) :: p
        real, intent(in) :: x
        real, intent(out) :: r
        r = p%c * x
    end subroutine

    pure subroutine scal(s, x, r)
        real, intent(inout) :: s
        real, intent(in) :: x
        real, intent(out) :: r
        r = s * x
    end subroutine
end module

program gpu_liveout_12
    use gpu_liveout_12_m
    implicit none
    integer, parameter :: n = 1000
    type(par_t) :: p
    real :: x(n), b(n), d(n), s
    integer :: i
    do i = 1, n
        x(i) = i
    end do
    p%c = 3
    p%ncalls = 0
    s = 2
    do concurrent (i = 1:n)
        call step(p, x(i), b(i))
        call scal(s, x(i), d(i))
    end do
    print *, b(7), d(7)
    if (b(7) /= 21) error stop 1
    if (d(7) /= 14) error stop 2
end program
