subroutine find_fit(data_y)
real, intent(inout) :: data_y(2)
real :: tol, fvec(2)

tol = sqrt(epsilon(1.0)) * 1e4
fvec = tol
call fcn(fvec)

contains

    subroutine fcn(fvec)
        real :: fvec(:)
        data_y = fvec
    end subroutine fcn

end subroutine find_fit

program associate_10

real :: datay(2)

datay = 4.0
call find_fit(datay)
print *, datay
if( abs(datay(1) - 3.45266986) > 1e-6 ) error stop
if( abs(datay(2) - 3.45266986) > 1e-6 ) error stop

end program associate_10
