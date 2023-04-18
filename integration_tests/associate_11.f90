program main
double precision :: data_y(1)
double precision :: tol, fvec(1)

data_y  = 1.0
tol = sqrt(epsilon(1.0))
call fcn(fvec)
if (abs(fvec(1) - 1.0) > 1e-12) error stop
print *, fvec
contains

    subroutine fcn(fvec)
    double precision :: fvec(1)
    fvec = data_y
    end subroutine fcn

end program main
