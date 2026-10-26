! A real that holds a NaN before a `do concurrent` whose every iteration
! passes it to a procedure that defines it in one iteration only: a
! thread whose call leaves the NaN as it was does not put it back over the
! value the defining iteration gave the variable.
module gpu_liveout_10_m
    implicit none
contains
    pure subroutine maybe_set(x, i)
        real, intent(inout) :: x
        integer, intent(in) :: i
        if (i == 7) x = 7.5
    end subroutine

    real function quotient(a, b)
        real, intent(in) :: a, b
        quotient = a / b
    end function
end module

program gpu_liveout_10
    use gpu_liveout_10_m
    implicit none
    integer, parameter :: n = 100000
    real :: x
    integer :: i
    integer, allocatable :: a(:)
    allocate(a(n))
    x = quotient(0.0, 0.0)
    if (x == x) error stop 1
    do concurrent (i = 1:n)
        a(i) = i
        call maybe_set(x, i)
    end do
    print *, x
    if (x /= 7.5) error stop 2
end program
