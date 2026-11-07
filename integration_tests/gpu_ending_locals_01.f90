! A module subroutine whose second launch reads a local array the first one
! writes, and one whose launch reads an optional argument, return without
! waiting for the launches: where those locals end, the runtime waits only for
! a launch that uses their storage in place. The large size makes the local of
! `two_steps` big enough for the Metal runtime to use it in place.
module gpu_ending_locals_01_m
implicit none
contains
subroutine two_steps(x, y, shift)
    real, contiguous, intent(in) :: x(:)
    real, contiguous, intent(out) :: y(:)
    real, intent(in) :: shift
    real :: t(size(x))
    integer :: i
    do concurrent (i = 1:size(x))
        t(i) = 2*x(i) + shift
    end do
    do concurrent (i = 1:size(x))
        y(i) = t(i) - x(i)
    end do
end subroutine
subroutine shifted(x, y, bias)
    real, contiguous, intent(in) :: x(:)
    real, contiguous, intent(out) :: y(:)
    real, contiguous, intent(in), optional :: bias(:)
    integer :: i
    do concurrent (i = 1:size(x))
        y(i) = x(i)
        if (present(bias)) y(i) = y(i) + bias(i)
    end do
end subroutine
end module

program gpu_ending_locals_01
use gpu_ending_locals_01_m
implicit none
integer, parameter :: sizes(2) = [100, 2**19]
real, allocatable :: x(:), y(:), z(:), w(:)
integer :: i, k, rep
do k = 1, size(sizes)
    allocate(x(sizes(k)), y(sizes(k)), z(sizes(k)), w(sizes(k)))
    do i = 1, sizes(k)
        x(i) = real(i)
    end do
    do rep = 1, 3
        call two_steps(x, y, real(rep))
        call two_steps(y, z, real(10*rep))
        call shifted(z, w, x)
        call shifted(w, y)
        do i = 1, sizes(k)
            if (z(i) /= x(i) + 11*rep) error stop "two_steps"
            if (w(i) /= 2*x(i) + 11*rep) error stop "shifted with bias"
            if (y(i) /= w(i)) error stop "shifted without bias"
        end do
    end do
    deallocate(x, y, z, w)
end do
print *, "ok"
end program
