program example_lmdif1
implicit none

real :: x(3), err
x = 4.0

err = abs(sum2(x) - 12.0)
print *, err
if (err > 1e-6) error stop

contains

    real function sum2(x) result(r)
    real, intent(in) :: x(:)
    integer :: i
    r = 0
    do i = 1, size(x)
        r = r + x(i)
    end do
    end function

end program
