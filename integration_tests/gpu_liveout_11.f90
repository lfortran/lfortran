! Type-bound subroutines called in a `do concurrent`, whose passed object
! and scalar arguments are `intent(in)`: neither is defined by the loop,
! so each keeps the value it had before it, whether the object is a local
! of the program or a polymorphic dummy argument of a type-bound procedure
! holding the loop.
module gpu_liveout_11_m
    implicit none
    type :: model_t
        real :: c
    contains
        procedure :: eval
        procedure :: run
        procedure :: apply
    end type
contains
    pure subroutine eval(self, x, y)
        class(model_t), intent(in) :: self
        real, intent(in) :: x
        real, intent(out) :: y
        y = self%c * x
    end subroutine

    pure subroutine apply(self, s, x, r)
        class(model_t), intent(in) :: self
        real, intent(in) :: s, x
        real, intent(out) :: r
        r = self%c * s * x
    end subroutine

    subroutine run(self, x, y)
        class(model_t), intent(in) :: self
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(x))
            call self%eval(x(i), y(i))
            y(i) = y(i) + self%c
        end do
    end subroutine
end module

program gpu_liveout_11
    use gpu_liveout_11_m
    implicit none
    integer, parameter :: n = 1000
    type(model_t) :: m
    real :: x(n), y(n), b(n), s
    integer :: i
    do i = 1, n
        x(i) = i
    end do
    m%c = 3
    do concurrent (i = 1:n)
        call m%eval(x(i), y(i))
    end do
    print *, y(7)
    if (y(7) /= 21) error stop 1
    s = 2
    do concurrent (i = 1:n)
        call m%apply(s, x(i), b(i))
    end do
    print *, b(7)
    if (b(7) /= 42) error stop 2
    call m%run(x, y)
    print *, y(7)
    if (y(7) /= 24) error stop 3
end program
