subroutine where_03
    implicit none
    real a(4)
    real b(4)
    
    a = (/ 1.0, 2.0, 3.0, 4.0/)
    b = (/ -1.0, -2.0, 5.0, 7.0/)
    where (a > b) 
        a = 1.0
    endwhere
    
    where(a == 1.0)
        a = 2.0
    else where(a == 2.0)
        b = 3.0
    else where
        a = b * 2.0 / a * 3.0
    endwhere

    if (abs(a(1) - 2.0) > 1e-7  ) error stop
    if (abs(a(2) - 2.0) > 1e-7  ) error stop
    if (abs(a(3) - 10.0) > 1e-7  ) error stop
    if (abs(a(4) - 10.5) > 1e-7  ) error stop

    if (abs(b(1) - (-1.0)) > 1e-7  ) error stop
    if (abs(b(2) - (-2.0)) > 1e-7  ) error stop
    if (abs(b(3) - 5.0) > 1e-7  ) error stop
    if (abs(b(4) - 7.0) > 1e-7  ) error stop

    print *, a
    print *, b


end subroutine

program main
call where_03
end program
