program main
    double precision, external :: enorm
    print *, enorm(1.0d0)
end program

double precision function enorm(n) result(y)
    double precision, intent(in) :: n
    y = n
    return
end function
