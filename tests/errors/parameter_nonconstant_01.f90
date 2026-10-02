program parameter_nonconstant_01
    integer :: bla
    integer, parameter :: y = abs(bla)
    print *, y
end program
