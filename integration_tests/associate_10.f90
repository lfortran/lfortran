program main
real :: data_y(1)
real :: tol, fvec(1)

data_y  = 1.0
tol = sqrt(epsilon(1.0))
call fcn(fvec)
if (abs(fvec(1) - 1.0) > 1e-7) error stop
print *, fvec
contains

    subroutine fcn(fvec)
    real :: fvec(1)
    fvec = data_y
    end subroutine fcn

end program main
