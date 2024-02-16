program prec_and_range
    
    real(kind=4) :: x(2)
    complex(kind=8) :: y
  
    print*, precision(1.0)
    if (precision(1.0) /= 6) error stop
    print *, precision(x)
    if (precision(x) /= 6) error stop
    print *, precision(1.0d0)
    if (precision(1.0d0) /= 15) error stop
    print *, precision(y)
    if (precision(y) /= 15) error stop

end program prec_and_range