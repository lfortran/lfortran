       program main
           intrinsic aimag, cmplx, real
           !TODO for the below: runtime ComplexConstructor not
           !implemented yet
           complex f
           f = cmplx(3, 1)
           complex :: x = (1.0, -3.0)
           real :: y
           y = aimag(x)
           intrinsic mod
       end program

