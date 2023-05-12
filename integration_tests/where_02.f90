program main
    implicit none
    real a(2)
    real b(2)
    
    a(1) = 1.0
    a(2) = 2.0

    b(1) = 0.5
    b(2) = 3.5

    where (a > b) 
        a = 1.5
    endwhere
    
    print *, a

    if (abs(a(1) - 1.5) > 1e-7) error stop
    if (abs(a(2) - 2.0) > 1e-7) error stop
end program
