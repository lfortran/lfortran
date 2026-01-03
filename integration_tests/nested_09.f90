program nested_09
implicit none
integer :: i
real :: x, fvec(5)
x = 1
call fcn(x, fvec)
print *, fvec
do i = 1, 5
    if ((abs(fvec(i)) - 1.0) > 1e-6) error stop
end do

contains

    subroutine check_deriv()
        call fcn(x, fvec)
    end subroutine check_deriv

    subroutine fcn(x, fvec)
        real, intent(in) :: x
        real, intent(out) :: fvec(5)
        fvec = x
    end subroutine fcn

end program
